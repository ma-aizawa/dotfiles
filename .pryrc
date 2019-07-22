# https://qiita.com/yujinakayama/items/6eda098e19d5f232976e
Pry.config.prompt = Pry.config.prompt.map do |default_proc|
  proc do |target_self, nest_level, pry|
    if defined?(RSpec::Core::ExampleGroup) && target_self.is_a?(RSpec::Core::ExampleGroup)
      target_self = RSpec::Core::ExampleGroup
    end

    default_proc.call(target_self, nest_level, pry)
  end
end
