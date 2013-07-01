# encoding: utf-8
require 'spec_helper'

feature 'dish view' do
    scenario 'requires valid user' do
        visit '/dish'
        login_pending_user
        page.should have_content('Unauthorized')
    end

    scenario "prevents viewing other user's dishes" do
        visit '/'
        click_on 'Login'
        login_valid_user
        page.should have_content('Dishes')

        # the login dialog won't return to a 404; so log in and visit it manually
        visit '/dish/validuser2+entree'
        page.should have_content('Page Not Found')
    end

    scenario "prevents viewing other user's dishes" do
        visit '/'
        click_on 'Login'
        login_valid_user
        page.should have_content('Dishes')

        # the login dialog won't return to a 404; so log in and visit it manually
        visit '/dish/a+non+existant+dish'
        page.should have_content('Page Not Found')
    end

    context 'assuming valid user' do
        background do
            visit '/dish/entree+1'
            login_valid_user
        end

        scenario 'is visible' do
            page.should have_content('Entree 1')
        end

        scenario 'can set type' do
            choose 'Side'
            page.should have_content('Saved')
            visit '/dish/entree+1'
            page.should have_checked_field('Side')
            choose 'Entrée'
            page.should have_content('Saved')
            visit '/dish/entree+1'
            page.should have_checked_field('Entrée')
        end

        scenario 'can set servings' do
            fill_in 'Servings', :with => '5'
            page.execute_script("$('#servings').trigger('change');")
            page.should have_content('Saved')
            visit '/dish/entree+1'
            page.should have_field('Servings', with: '5')
        end
    end
end
