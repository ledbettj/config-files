# -*- mode: ruby; -*-

# Keep pry history in each project separately.
proj = Pathname.getwd.ascend.find do |path|
  git = path.join('.git')
  path.writable? && git.directory?
end

Pry.config.history.file = proj.join('.pry_history').to_s unless proj.nil?
