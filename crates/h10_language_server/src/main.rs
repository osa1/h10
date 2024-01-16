#![allow(clippy::field_reassign_with_default)]

mod buffer;
mod token;

use buffer::{Buffer, Token};
use token::TOKEN_TYPES;

use std::fs::File;
use std::io::Write;
use std::sync::Mutex;

use serde_json::Value;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::request::*;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

fn main() {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let log_file_path = {
        let mut path = dirs::home_dir().unwrap();
        path.push("h10_language_server_logs");
        path
    };
    let log_file = File::create(log_file_path).unwrap();
    rt.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let (service, socket) = LspService::new(|client| Backend {
            client,
            log_file: Mutex::new(log_file),
            file_uri: Mutex::new(None),
            buffer: Mutex::new(None),
        });
        Server::new(stdin, stdout, socket).serve(service).await;
    });
}

#[derive(Debug)]
struct Backend {
    #[allow(unused)]
    client: Client,
    log_file: Mutex<File>,
    file_uri: Mutex<Option<Url>>,
    buffer: Mutex<Option<Buffer>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        writeln!(self.log_file.lock().unwrap(), "initialize").unwrap();

        let capabilities = ServerCapabilities {
            // Sync documents incrementally.
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::INCREMENTAL),
                    will_save: None,
                    will_save_wait_until: None,
                    save: Some(SaveOptions::default().into()),
                },
            )),

            // Declare "go to declaration" support.
            declaration_provider: Some(DeclarationCapability::Simple(true)),

            // Declare semantic syntax highlighting support.
            semantic_tokens_provider: Some(
                SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        // TODO: This needs to be in sync with `to_lsp_token_type` values.
                        token_types: TOKEN_TYPES.into(),
                        token_modifiers: vec![],
                    },

                    // Support full document highlighting.
                    full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),

                    // Support range highlighting.
                    range: Some(true),

                    // Dunno why we have this in this type?
                    work_done_progress_options: Default::default(),
                }
                .into(),
            ),

            // Declare symbol list/outline support.
            document_symbol_provider: Some(OneOf::Left(true)),

            ..Default::default()
        };

        Ok(InitializeResult {
            capabilities,
            ..Default::default()
        })
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        let uri = params.text_document.uri;
        *self.buffer.lock().unwrap() = Some(Buffer::with_contents(&params.text_document.text));
        *self.file_uri.lock().unwrap() = Some(uri);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        let mut buffer_lock = self.buffer.lock().unwrap();
        let buffer: &mut Buffer = buffer_lock.as_mut().unwrap();

        for change in params.content_changes {
            let range = match &change.range {
                Some(range) => range,
                None => {
                    write!(self.log_file.lock().unwrap(), "did_change without range").unwrap();
                    return;
                }
            };

            let text = change.text;

            buffer.remove(&mut Default::default(), range.start, range.end);
            buffer.insert(range.start, &text);
        }
    }

    // Note: VSCode doesn't call this after every modification, we can generate the result lazily.
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Ok(None)
        // let url: Url = self.file_uri.lock().unwrap().clone().unwrap();
        // Ok(Some(DocumentSymbolResponse::Flat(vec![
        //     SymbolInformation {
        //         name: "test".to_owned(),
        //         kind: SymbolKind::FUNCTION,
        //         tags: None,
        //         location: Location::new(
        //             url,
        //             Range {
        //                 start: Position {
        //                     line: 0,
        //                     character: 0,
        //                 },
        //                 end: Position {
        //                     line: 0,
        //                     character: 0,
        //                 },
        //             },
        //         ),
        //         container_name: None,
        //         deprecated: None,
        //     },
        // ])))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        writeln!(
            self.log_file.lock().unwrap(),
            "semantic_tokens_full({:#?})",
            params
        )
        .unwrap();

        let buffer_lock = self.buffer.lock().unwrap();
        let buffer: &Buffer = buffer_lock.as_ref().unwrap();

        // 200,000 bytes.
        let mut data: Vec<SemanticToken> = Vec::with_capacity(10_000);

        for (
            line_idx,
            Token {
                kind,
                col,
                length_cols,
            },
        ) in buffer.iter_tokens_from(0, 0)
        {
            let lsp_token = match kind.to_lsp_token_type() {
                None => continue,
                Some(lsp_token) => lsp_token,
            };

            data.push(SemanticToken {
                delta_line: line_idx + 1,
                delta_start: col,
                length: length_cols,
                token_type: lsp_token,
                token_modifiers_bitset: 0,
            });
        }

        writeln!(self.log_file.lock().unwrap(), "{:?}", data).unwrap();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    // TODO
    async fn semantic_tokens_full_delta(
        &self,
        params: SemanticTokensDeltaParams,
    ) -> Result<Option<SemanticTokensFullDeltaResult>> {
        writeln!(
            self.log_file.lock().unwrap(),
            "semantic_tokens_full_delta({:#?})",
            params
        )
        .unwrap();
        Err(Error::method_not_found())
    }

    // TODO
    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        writeln!(
            self.log_file.lock().unwrap(),
            "semantic_tokens_range({:#?})",
            params
        )
        .unwrap();
        Err(Error::method_not_found())
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Placeholders below

    async fn initialized(&self, _: InitializedParams) {
        writeln!(self.log_file.lock().unwrap(), "initialized").unwrap();
    }

    async fn shutdown(&self) -> Result<()> {
        writeln!(self.log_file.lock().unwrap(), "shutdown").unwrap();
        Ok(())
    }

    async fn will_save(&self, params: WillSaveTextDocumentParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn will_save_wait_until(
        &self,
        params: WillSaveTextDocumentParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn goto_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> Result<Option<GotoDeclarationResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> Result<Option<GotoImplementationResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn document_link(&self, params: DocumentLinkParams) -> Result<Option<Vec<DocumentLink>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn document_link_resolve(&self, params: DocumentLink) -> Result<DocumentLink> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn code_lens_resolve(&self, params: CodeLens) -> Result<CodeLens> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn inline_value(&self, params: InlineValueParams) -> Result<Option<Vec<InlineValue>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn inlay_hint_resolve(&self, params: InlayHint) -> Result<InlayHint> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn moniker(&self, params: MonikerParams) -> Result<Option<Vec<Moniker>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn workspace_diagnostic(
        &self,
        params: WorkspaceDiagnosticParams,
    ) -> Result<WorkspaceDiagnosticReportResult> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn code_action_resolve(&self, params: CodeAction) -> Result<CodeAction> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn document_color(&self, params: DocumentColorParams) -> Result<Vec<ColorInformation>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn color_presentation(
        &self,
        params: ColorPresentationParams,
    ) -> Result<Vec<ColorPresentation>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn on_type_formatting(
        &self,
        params: DocumentOnTypeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn linked_editing_range(
        &self,
        params: LinkedEditingRangeParams,
    ) -> Result<Option<LinkedEditingRanges>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn symbol_resolve(&self, params: WorkspaceSymbol) -> Result<WorkspaceSymbol> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn will_create_files(&self, params: CreateFilesParams) -> Result<Option<WorkspaceEdit>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn did_create_files(&self, params: CreateFilesParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn will_rename_files(&self, params: RenameFilesParams) -> Result<Option<WorkspaceEdit>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn did_rename_files(&self, params: RenameFilesParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn will_delete_files(&self, params: DeleteFilesParams) -> Result<Option<WorkspaceEdit>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        writeln!(self.log_file.lock().unwrap(), "{:#?}", params).unwrap();
        Err(Error::method_not_found())
    }
}
