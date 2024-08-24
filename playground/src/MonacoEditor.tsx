import * as monaco from "monaco-editor";
import { useCallback, useEffect, useRef } from "react";

export function MonacoEditor({ model }: { model: monaco.editor.ITextModel }) {
	const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

	useEffect(() => {
		editorRef.current?.setModel(model);
	}, [model]);

	const containerRef = useCallback(
		(ref: HTMLDivElement | null) => {
			console.log("!!!!!!!!");

			if (ref === null) {
				return;
			}

			const editor = monaco.editor.create(ref, {
				value: 'print("Hello, World!")',
				language: "text",
				theme: "vs",
				automaticLayout: true,
				minimap: { enabled: false },
				fontSize: 18,
			});
			editor.setModel(model);

			editorRef.current = editor;
		},
		[model],
	);

	return (
		<div
			ref={containerRef}
			css={{
				position: "relative",
				width: "100%",
				height: "100%",
			}}
		/>
	);
}
