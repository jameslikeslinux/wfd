require 'spec_helper'

feature 'dishes list' do
    scenario 'requires valid user' do
        visit '/dishes'
        login_pending_user
        page.should have_content('Unauthorized')
    end

    scenario 'is visible for valid user' do
        visit '/dishes'
        login_valid_user
        page.should have_content('New Dish')
    end
end
