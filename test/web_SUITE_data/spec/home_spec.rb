require 'spec_helper'

feature 'home page' do
    scenario 'prompts non-logged-in user to login or register' do
        visit '/'
        page.should have_content('Login')
        page.should have_content('Register')
    end

    scenario 'prompts pending user to validate email address' do
        visit '/'
        click_on 'Login'
        login_pending_user
        page.should have_content('You must validate your e-mail address before using this app.')
    end

    scenario 'shows all options to valid users' do
        visit '/'
        click_on 'Login'
        login_valid_user
        page.should have_content('Dishes')
        page.should have_content('Menus')
        page.should have_content('Go Shopping')
    end
end
