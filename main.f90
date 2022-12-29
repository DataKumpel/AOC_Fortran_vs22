program main
    implicit none
    !===== Variables =========================================================!
    
    !===== Variables =========================================================!
    
    write(*, *) "===== DAY  8 ====="
    call day_08()
    
    write(*, *) "===== DAY  9 ====="
    call day_09()
    
    write(*, *) "===== DAY 10 ====="
    call day_10()
end program main


subroutine day_08()
    use challenge_08
    implicit none
    !=========================================================================!
    integer :: grid_width = 0
    integer :: grid_height = 0
    integer :: num_vis_trees = 0
    integer :: highest_scenic_score = 0

    integer, dimension(:, :), allocatable :: tree_grid
    
    logical :: test = .false.
    !=========================================================================!
    
    if(.not. test) then
        call get_gridsize("input_08.txt", grid_width, grid_height)
        allocate(tree_grid(grid_height, grid_width))
        call read_tree_grid("input_08.txt", grid_width, grid_height, tree_grid)
    else
        call get_gridsize("input_08_test.txt", grid_width, grid_height)
        allocate(tree_grid(grid_height, grid_width))
        call read_tree_grid("input_08_test.txt", grid_width, grid_height, &
                           &tree_grid)
    endif
    
    call challenge_part_i(tree_grid, grid_width, grid_height)
    call challenge_part_ii(tree_grid, grid_width, grid_height)
end subroutine day_08


subroutine day_09()
    use challenge_09
    implicit none
    !=========================================================================!
    logical :: test = .false.
    real :: t_start, t_end
    !=========================================================================!
    
    call cpu_time(t_start)
    if(test) then
        call challenge_part_i("input_09_test_p1.txt")
        call challenge_part_ii("input_09_test_p2.txt")
    else
        call challenge_part_i("input_09.txt")
        call challenge_part_ii("input_09.txt")
    endif
    call cpu_time(t_end)
    
    write(*, "(A, F10.8, A)") "Exec time: ", t_end - t_start, "s"
end subroutine day_09


subroutine day_10()
    use challenge_10
    implicit none
    !=========================================================================!
    logical :: test = .false.
    !=========================================================================!
    
    if(test) then
        call challenge_part_i("input_10_test.txt")
        call challenge_part_ii("input_10_test.txt")
    else
        call challenge_part_i("input_10.txt")
        call challenge_part_ii("input_10.txt")
    endif
    
end subroutine day_10