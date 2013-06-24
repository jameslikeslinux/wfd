require 'spec_helper'

feature 'home page' do
    scenario 'prompts user to login or register' do
        visit '/'
        page.should have_content('Login')
        page.should have_content('Register')
    end
end
