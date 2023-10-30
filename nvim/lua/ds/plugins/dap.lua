return {
	"mfussenegger/nvim-dap",
	{
		"rcarriga/nvim-dap-ui",
		lazy = true,
		dependencies = { "mfussenegger/nvim-dap" },
		config = function()
			require("dapui").setup()
		end,
	},
	{
		"leoluz/nvim-dap-go",
		lazy = true,
		config = function()
			require("dap-go").setup()
		end,
	},
	{
		"jbyuki/one-small-step-for-vimkind",
		dependencies = { "mfussenegger/nvim-dap" },
	},
	{
		"theHamsta/nvim-dap-virtual-text",
		dependencies = { "mfussenegger/nvim-dap", "nvim-treesitter/nvim-treesitter" },
		config = function()
			require("nvim-dap-virtual-text").setup()
		end,
	},
}
