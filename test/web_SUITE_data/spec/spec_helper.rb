require 'capybara/rspec'
require 'capybara-screenshot'
require 'capybara-screenshot/rspec'

Capybara.default_driver = :selenium
Capybara.app_host = 'http://localhost:8082'
Capybara.default_wait_time = 5

module Capybara
    class Session
        ##
        #
        # Retry executing the block until a truthy result is returned or the timeout time is exceeded
        #
        # @param [Integer] timeout The amount of seconds to retry executing the given block
        #
        # this method was removed in Capybara v2 so adding it back if not already defined
        #
        unless defined?(wait_until)
            def wait_until(timeout = Capybara.default_wait_time)
                Capybara.send(:timeout, timeout, driver) { yield }
            end
        end
    end
end

def wait_for_ajax
    page.wait_until { page.evaluate_script('$.active') == 0 }
end

def login_pending_user
    fill_in 'Username', :with => 'pendinguser'
    fill_in 'Password', :with => 'password'
    click_on 'Log In'
    wait_for_ajax
end

def login_valid_user
    fill_in 'Username', :with => 'validuser1'
    fill_in 'Password', :with => 'password'
    click_on 'Log In'
    wait_for_ajax
end
