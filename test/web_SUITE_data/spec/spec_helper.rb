require 'capybara/rspec'

Capybara.default_driver = :selenium
Capybara.app_host = 'http://localhost:8082'
Capybara.default_wait_time = 5

def login_pending_user
#    visit '/login'
    fill_in 'Username', :with => 'pendinguser'
    fill_in 'Password', :with => 'password'
    click_on 'Login'
end

def login_valid_user
#    visit '/login'
    fill_in 'Username', :with => 'validuser'
    fill_in 'Password', :with => 'password'
    click_on 'Login'
end
