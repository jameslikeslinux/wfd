# encoding: utf-8
require 'spec_helper'

feature 'dishes list' do
    scenario 'requires valid user' do
        visit '/dishes'
        login_pending_user
        page.should have_content('Unauthorized')
    end

    context 'assuming valid user' do
        background do
            visit '/dishes'
            login_valid_user
        end

        scenario 'is visible' do
            page.should have_content('Entree 1')
            page.should have_content('Side 1')
            page.should_not have_content('validuser2 Entree')
        end

        scenario 'can create new dish' do
            click_on 'New Dish'
            wait_for_ajax
            fill_in 'Dish Name', :with => 'Dish 1'
            click_on 'Create'
            wait_for_ajax
            page.evaluate_script('window.history.back()')
            page.should have_content('Dish 1')
        end

        scenario "can't create duplicate dish" do
            click_on 'New Dish'
            wait_for_ajax
            fill_in 'Dish Name', :with => 'Entree 1'
            click_on 'Create'
            wait_for_ajax
            page.should have_content('Dish already exists')
        end

        scenario 'will show only entrees' do
            choose 'Entrées'
            wait_for_ajax
            page.should have_content('Entree 1')
            page.should_not have_content('Side 1')
        end

        scenario 'will show only sides' do
            choose 'Sides'
            wait_for_ajax
            page.should have_content('Side 1')
            page.should_not have_content('Entree 1')
        end

        scenario 'will filter dishes' do
            click_on 'Search'
            wait_for_ajax
            fill_in 'Filter items...', :with => '2'
            page.should have_content('Entree 2')
            page.should have_content('Side 2')
            page.should_not have_content('Entree 1')
            page.should_not have_content('Side 1')
        end
    end
end
