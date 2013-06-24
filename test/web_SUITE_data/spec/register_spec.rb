require 'spec_helper'

feature 'registration' do
    scenario "doesn't let the user enter nothing" do
        visit '/register'
        click_on 'Register'
        page.should have_content('Required')
    end

    scenario "doesn't let the user enter an invalid username" do
        visit '/register'
        fill_in 'Username', :with => 'a'
        click_on 'Register'
        page.should have_content('Must have at least 3 characters')
    end
end
