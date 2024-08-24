import { createRoot } from "react-dom/client";
import { App } from "./App";

const container = document.getElementById("root");
if (container === null) {
	alert("Failed to initialize application");
} else {
	const root = createRoot(container);
	root.render(<App />);
}
