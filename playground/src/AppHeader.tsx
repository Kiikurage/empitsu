import type { AnchorHTMLAttributes, ReactNode } from "react";

export function AppHeader({
	onRunButtonClick,
	onSaveButtonClick,
	onHelpButtonClick,
}: {
	onRunButtonClick?: () => void;
	onSaveButtonClick?: () => void;
	onHelpButtonClick?: () => void;
}) {
	return (
		<header
			css={{
				display: "flex",
				flexDirection: "row",
				alignItems: "center",
				justifyContent: "space-between",
				gap: 16,
				padding: "0 32px",
				height: 48,
				background: "#da7b2d",
			}}
		>
			<div
				css={{
					padding: 0,
					display: "flex",
					flexDirection: "row",
					gap: 16,
				}}
			>
				<h1
					css={{
						padding: 0,
						fontSize: "1.25rem",
						color: "#fff",
						whiteSpace: "nowrap",
						overflow: "hidden",
						textOverflow: "ellipsis",
					}}
				>
					Empitsu Playground
				</h1>
				<AppHeaderButtonLink
					href="https://github.com/Kiikurage/empitsu"
					target="_blank"
				>
					<img
						src={require("./github-mark-white.svg")}
						alt="GitHub"
						width={26}
					/>
					<span className="sm-hide">GitHub</span>
				</AppHeaderButtonLink>
			</div>
			<div
				css={{
					padding: 0,
					display: "flex",
					flexDirection: "row",
					gap: 16,
				}}
			>
				<AppHeaderButton onClick={onRunButtonClick}>
					<span className="material-symbols-outlined">play_arrow</span>
					<span className="sm-hide">Run</span>
				</AppHeaderButton>
				<AppHeaderButton onClick={onSaveButtonClick}>
					<span className="material-symbols-outlined">save</span>
					<span className="sm-hide">Save</span>
				</AppHeaderButton>
				<AppHeaderButton onClick={onHelpButtonClick}>
					<span className="material-symbols-outlined">help</span>
					<span className="sm-hide">Help</span>
				</AppHeaderButton>
			</div>
		</header>
	);
}

export function AppHeaderButton({
	children,
	onClick,
}: {
	children?: ReactNode;
	onClick?: () => void;
}) {
	return (
		<button
			css={{
				padding: "8px 16px",
				background: "transparent",
				color: "#fff",
				border: "none",
				borderRadius: 4,
				cursor: "pointer",
				fontSize: "1.25rem",
				display: "flex",
				flexDirection: "row",
				alignItems: "center",
				justifyContent: "center",
				gap: 8,
				":hover": {
					background: "rgba(255,255,255,0.2)",
				},
				":active": {
					background: "rgba(255,255,255,0.35)",
				},
			}}
			type="button"
			onClick={onClick}
		>
			{children}
		</button>
	);
}

export function AppHeaderButtonLink({
	children,
	...otherAttributes
}: AnchorHTMLAttributes<HTMLAnchorElement> & {
	children?: ReactNode;
}) {
	return (
		<a
			css={{
				padding: "8px 16px",
				background: "transparent",
				color: "#fff",
				border: "none",
				borderRadius: 4,
				cursor: "pointer",
				fontSize: "1.25rem",
				display: "flex",
				textDecoration: "none",
				flexDirection: "row",
				alignItems: "center",
				justifyContent: "center",
				gap: 8,
				":hover": {
					background: "rgba(255,255,255,0.2)",
				},
				":active": {
					background: "rgba(255,255,255,0.35)",
				},
			}}
			{...otherAttributes}
		>
			{children}
		</a>
	);
}
