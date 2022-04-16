program HuntTheWumpus

   implicit none      
   integer :: inGame

   inGame = 1
   call game(inGame)
end program HuntTheWumpus
      
! Main game subroutine, seperate from main program to allow
! player to play again with the same game board without exiting program   
subroutine game(inGame)

   implicit none
   character(len = 1) :: instr, moveShoot    
   integer :: hunter, arrow, numArrows, wumpus, bat1, bat2, inGame
   integer :: pit1, pit2
   integer, dimension(1:20,1:3) :: cave

   write(*,*) 'Welcome! Do you want to read the instructions? (Y-N)? '
   read (*,*) instr 

   do while (.not.((instr == 'Y') .or. (instr == 'y')&
      &.or.(instr == 'N') .or. (instr == 'n')))
         write(*,*) 'Invalid input. Please Choose between Y and N:'
         read (*,*) instr
   end do

   if((instr == 'Y') .or. (instr == 'y')) then
         call Instructions
   else if((instr == 'N') .or. (instr == 'n')) then
         continue
   end if      

   call startGame(hunter, bat1, bat2, pit1, pit2, wumpus, cave,&
   & numArrows)

   do while(inGame == 1)
      write(*,*)''
      write(*,*) cave(hunter, 1), cave(hunter, 2), cave(hunter, 3)
      write(*,*)''
      write(*,*)'          ^           ^          ^'
      write(*,*)'          ^           ^          ^'
      write(*,*)'          ^           ^          ^'
      write(*,*)'           ______________________'
      write(*,*)'          |                      |'
      write(*,*)'          |                      |'
      write(*,*)'          |', hunter, '         |'
      write(*,*)'          |                      |'
      write(*,*)'          |______________________|'
      write(*,*)''
      write(*,*)''

      !Print any hazard detections
      
      ! Detect bats
      if((bat1 == cave(hunter,1)) .or.&
      & (bat2 == cave(hunter,1)) .or.&
      & (bat1 == cave(hunter,2)) .or.&
      & (bat2 == cave(hunter,2)) .or.&
      & (bat1 == cave(hunter,2)) .or.&
      & (bat2 == cave(hunter,3))) then
         write(*,*)'Bats near you...'
      end if
      
      ! Detect pit
      if((pit1 == cave(hunter,1)) .or.&
      & (pit2 == cave(hunter,1)) .or.&
      & (pit1 == cave(hunter,2)) .or.&
      & (pit2 == cave(hunter,2)) .or.&
      & (pit1 == cave(hunter,2)) .or.&
      & (pit2 == cave(hunter,3))) then
         write(*,*)'You feel a draft...'
      end if
      
      ! Detect wumpus
      if((wumpus== cave(hunter,1)) .or.&
      &   (wumpus == cave(hunter,2)) .or.&
      &   (wumpus == cave(hunter,3))) then
         write(*,*)'I smell a Wumpus..'
      end if


      write(*,*)'You are in room:', hunter
      write(*,*)'Which leads to:', cave(hunter,:)
      write(*,*)'Do you wanna move or shoot? S or M'

      
   10 read(*,*) moveShoot

      if((moveShoot == 's') .or. (moveShoot == 'S')) then
         call shoot(hunter, wumpus, cave, arrow, numArrows)
      else if((moveShoot == 'm') .or. (moveShoot == 'M')) then
         call move(hunter, bat1, bat2, pit1, pit2, wumpus, cave)
      else
         write(*,*)'Invalid input. Please choose S or M:'
         go to 10
      end if
   end do
end subroutine game


! Subroutine that prints the instructions of the game
subroutine instructions()
   write(*,*)''
   write(*,*)'Welcome to Hunt the Wumpus!! '
   write(*,*)'The Wumpus lives in a cave of 20 rooms. '
   write(*,*)'Each room has 3 tunnels leading to other rooms'
   write(*,*)'Look at a dodecahedron to see how it works'   
   write(*,*)'If you do not know what a dodecahedron is,'
   write(*,*)'ASK SOMEONE'
   write(*,*)''
   write(*,*)'HAZARDS:  '
   write(*,*)'Bottomless Pits: Two rooms have bottomless pits'
   write(*,*)'- If you go there, you fall into the pit and lose'
   write(*,*)'Super Bats: Two other rooms have super bats'
   write(*,*)'- If you go there, a bat will move you to a random room'
   write(*,*)'Wumpus:'
   write(*,*)'- The Wumpus is only bothered by you entering his room or'
   write(*,*)'  your arrow shooting into his room'
   write(*,*)'- If he is bothered, he may move to another room,'
   write(*,*)'  or choose to stay in the same room'
   write(*,*)'- If you are in the same room with the Wumpus,'
   write(*,*)'  he will eat you up, AND YOU LOSE'
   write(*,*)''
   write(*,*)'YOU:'
   write(*,*)'- Each turn you can move or shoot a crooked arrow'
   write(*,*)''
   write(*,*)'MOVE:'
   write(*,*)'- You can move one room (through one tunnel)'
   write(*,*)''
   write(*,*)'SHOOT: You have 5 arrows. YOU LOSE when you run out'
   write(*,*)'- Each arrow can go from 1 to 5 rooms, aim by telling'
   write(*,*)'  the computer the room #s you want the arrow to go to'
   write(*,*)'- If the arrow cannot go that way it moves at random '
   write(*,*)'  to the next room'
   write(*,*)'- If the arrow hits the Wumpus, YOU WIN'
   write(*,*)'- If the arrow hits you, YOU LOSE'
   write(*,*)''
   write(*,*)'WARNINGS: '
   write(*,*)'When you are one room away from the Wumpus or a Hazard'
   write(*,*)'the computer says:'
   write(*,*)'WUMPUS: I smell a Wumpus..'
   write(*,*)'BAT: Bats near you...'
   write(*,*)'PIT: You feel a draft...'
   write(*,*)''
   write(*,*)'***********************************************'
   write(*,*)'HUNT THE WUMPUS'

end subroutine instructions

      
      
!Builds the game board, places all the hazards and the player
subroutine startGame(hunter, bat1, bat2, pit1, pit2, wumpus, cave,&
&numArrows)
     
   implicit none    
   integer :: hunter, numArrows, wumpus, bat1, bat2
   integer :: pit1, pit2
   integer, dimension(1:20,1:3) :: cave

   ! Initialize our cave array with proper elements
   cave(1, :) = (/2, 5, 8/)
   cave(2, :) = (/1, 3, 10/)
   cave(3, :) = (/2, 4, 12/)
   cave(4, :) = (/3, 5, 14/)
   cave(5, :) = (/1, 4, 6/)
   cave(6, :) = (/5, 7, 15/)
   cave(7, :) = (/6, 8, 17/)
   cave(8, :) = (/1, 7, 9/)
   cave(9, :) = (/8, 10, 18/)
   cave(10, :) = (/2, 9, 11/)
   cave(11, :) = (/10, 12, 19/)
   cave(12, :) = (/3, 11, 13/)
   cave(13, :) = (/12, 14, 20/)
   cave(14, :) = (/4, 13, 15/)
   cave(15, :) = (/6, 14, 16/)
   cave(16, :) = (/15, 17, 20/)
   cave(17, :) = (/7 ,16, 18/)
   cave(18, :) = (/9, 17, 19/)
   cave(19, :) = (/11, 18, 20/)
   cave(20, :) = (/13, 16, 19/)

   numArrows = 5

   ! Initialize the hazards
   bat1 = int(rand(0) * 20) + 1
   bat2 = int(rand(0) * 20) + 1
   pit1 = int(rand(0) * 20) + 1
   pit2 = int(rand(0) * 20) + 1
   wumpus = int(rand(0) * 20) + 1
      
   ! Keep randomizing until they are unique
   do while ((bat1 == bat2) .or. (bat1 == pit1) & 
   & .or. (bat1 == pit2) .or. (bat2 == pit1) &
   & .or. (bat2 == pit2) .or. (pit1 == pit2) &
   & .or. (wumpus == bat1) .or. (wumpus == bat2) &
   & .or. (wumpus == pit1) .or. (wumpus == pit2))
      bat1 = int(rand(0) * 20) + 1
      bat2 = int(rand(0) * 20) + 1
      pit1 = int(rand(0) * 20) + 1
      pit2 = int(rand(0) * 20) + 1
      wumpus = int(rand(0) * 20) + 1     
   end do
   
   ! Initialize the player location until it is not on a hazard
   do while ((hunter == bat1).or.(hunter == bat2)&
   & .or.(hunter == pit1).or.(hunter == pit2)&
   & .or.(hunter == wumpus)) 
      hunter = int(rand(0) * 20) + 1
   end do
end subroutine startGame


! Subroutine for the end of the game, handles winning and
! losing cases and prompts user to play again 
subroutine endGame(curr)

   implicit none
   character(len = 1) :: input   
   integer :: inGame
   integer, dimension(1:3) ::  curr

   inGame = 0

   write(*,*)'Game over! Do you want to quit? Y or N'
20 read(*, *) input

   if(.not. ((input == 'Y') .or. (input == 'N')& 
   & .or. (input == 'y') .or. (input == 'n'))) then
      write(*, *)'Invalid input. Choose between Y or N:'
      read(*,*) input
      go to 20       
   else if((input == 'Y').or.(input == 'y')) then
      write(*,*)'Bye bye'
      stop
   else
      write(*,*)'Same board set up? Y or N'
21    read(*, *) input
      if(.not.((input == 'Y') .or. (input == 'N')& 
      & .or. (input == 'y').or. (input == 'n'))) then
         write(*,*)'Invalid input. Choose between Y or N'
         go to 21
         
      else if((input == 'N') .or. (input == 'n')) then
         inGame = 1
         call itime(curr) 
         call srand(curr(1) + curr(2) + curr(3))
         call game(inGame)

      else
         call game(inGame)
      end if
   end if
end subroutine endGame

      
! Subroutine to handle the wumpus being disturbed,
! shooting the wumpus and being eaten by the wumpus
subroutine checkWumpus(arrow, wumpus, cave, hunter)

   implicit none
   integer :: hunter, arrow, wumpus
   integer, dimension(1:20,1:3) :: cave
   integer, dimension(1:3) :: curr
    
   ! Check if the arrow reached the same room as the wumpus
   if(arrow == wumpus) then
      ! There is a chance the arrow wakes the wumpus and it moves
      if(rand() > 0.25) then
         wumpus=cave(wumpus,int(rand()*3)+1)
         write(*,*)'Wumpus was disturbed and moved to another room'
      else 
         ! If it doesn't move and it's in the room with the arrow
         ! Win condition for the game
         write(*,*)'You killed the Wumpus!'
         write(*,*)'Congratulations, you win!'
         write(*,*)'Hee hee hee - the Wumpus will get you next time...'
         call endGame(curr)
      end if
   end if
        
   ! Check if the hunter moves into the same room as the wumpus
   if(hunter == wumpus) then
      ! Chance that the wumpus moves
      if(rand() > 0.25) then
         write(*,*)'... Oops, bumped into a Wumpus!'
         wumpus = cave(wumpus,int(rand()*3)+1)
      else 
         ! Wumpus doesn't move and hunter is in the same room
         ! Lost the game
         write(*,*)'You were eaten by the Wumpus!'
         write(*,*)'Haha you lost!'
         call endGame(curr)
      end if
   end if 
end subroutine checkWumpus


! Handles all the logic for when the player elects to shoot
subroutine shoot(hunter, wumpus, cave, arrow, numArrows)

   implicit none 
   integer :: hunter, arrow, numArrows, wumpus
   integer :: numShoot, shootWhere, n
   integer, dimension(1:20,1:3) :: cave
   integer, dimension(1:3) :: curr
      
   arrow = hunter      
   numArrows = numArrows - 1

   write(*,*)'You can shoot up to 5 arrows. How many:'
30 read(*, *) numShoot
      
   ! Validate input
   if((numShoot > 5) .or. (numShoot < 1)) then
      write(*,*)'Wrong input. Choose between 1 and 5. How many: '
      go to 30
   end if

   ! Iterate through all arrows hunter wants to shoot
   do n = 1, numShoot
      write(*,*)'Where to?'
31    read(*, *) shootWhere

      if((shootWhere > 20) .or. (shootWhere < 1)) then
         write(*,*)'Arrows arent that crooked - try another room'
         go to 31
      end if


      if((shootWhere == cave(arrow,1)).or.(shootWhere == cave(arrow,2))& 
      & .or. (shootWhere == cave(arrow,3))) then
         arrow = shootWhere
         call checkWumpus(arrow, wumpus, cave, hunter)
         call shotSelf(hunter, arrow, curr)

      else
         arrow = cave(arrow, int(rand(0) * 3) + 1)
         write(*,*)'Arrow to a random direction!'

         call checkWumpus(arrow, wumpus, cave, hunter)
         call shotSelf(hunter, arrow, curr)
      end if
   end do

   write(*,*)'You missed!'

   if(numArrows == 0) then
      write(*,*)'You used up all of your arrows. You lose'
      write(*,*)'Haha you lost!'
      call endGame(curr)
   end if
      
end subroutine shoot


! Handles logic for when the player elects to move
subroutine move(hunter, bat1, bat2, pit1, pit2, wumpus, cave)

   implicit none
   integer :: hunter, arrow, wumpus, bat1, bat2
   integer :: pit1, pit2, room
   integer, dimension(1:20,1:3) :: cave
   integer, dimension(1:3) :: curr
   
   write(*,*)'Where to?'
40 read(*,*) room
     
   ! Check the hunter is moving to a legal room
   if((room == cave(hunter, 1)) .or. (room == cave(hunter, 2))& 
   & .or. (room == cave(hunter, 3))) then
      hunter = room
   else
      write(*,*)'You can only go to an adjacent room: '
      go to 40
   end if  

    
   ! Trip Bat Hazard
   do while((hunter == bat1) .or. (hunter == bat2)) 
      hunter = int(rand(0) * 20) + 1
      write(*,*)'Zap--Super bat snatch! Elsewhereville for you!'
   end do  

   ! Trip Pit Hazard	      
   if((hunter == pit1) .or. (hunter == pit2)) then
      write(*,*)'YYIIIIEEEE . . . Fell into a pit'
      write(*,*)'Haha you lost!'
      call endGame(curr)
   end if

   ! Trip Wumpus Hazard
   call checkWumpus(arrow, wumpus, cave, hunter)
end subroutine move


! Checks if the hunter and arrow are in the 
! same room meaning hunter shot self in the back
subroutine shotSelf(hunter, arrow, curr)

   implicit none
   integer :: hunter, arrow
   integer, dimension(1:3) :: curr

   if(hunter == arrow) then
      write(*,*)'You shot yourself in the back, game over!'
      write(*,*)'Haha you lost!'
      call endGame(curr)
   end if
end subroutine shotSelf

