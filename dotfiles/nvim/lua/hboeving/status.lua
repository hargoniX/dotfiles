require('el').reset_windows()

local builtin = require('el.builtin')
local extensions = require('el.extensions')
local sections = require('el.sections')
local subscribe = require('el.subscribe')

local git_branch = subscribe.buf_autocmd(
  "el_git_branch",
  "BufEnter",
  function(window, buffer)
    local branch = extensions.git_branch(window, buffer)
    if branch then
      return ' ' .. '' .. ' ' .. branch
    end
  end
)

local git_changes = subscribe.buf_autocmd(
  "el_git_changes",
  "BufWritePost",
  function(window, buffer)
    return extensions.git_changes(window, buffer)
  end
)

local generator = function()
    return {
        extensions.gen_mode {
            format_string = ' %s '
        },
        git_branch,
        " ",
        subscribe.buf_autocmd(
            "el_git_status",
            "BufWritePost",
            function(window, buffer)
                return extensions.git_changes(window, buffer)
            end
        ),
        sections.split,
        sections.split,

        sections.maximum_width(
            builtin.responsive_file(140, 90),
            0.30
        ),
        ':%l:%c',
    }
end

require 'el'.setup{
    generator = generator
}


