-- Seed: 1136814765146557130,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity kyvp is
  port (bwipsa : out std_logic; elc : buffer time);
end kyvp;

architecture pk of kyvp is
  
begin
  -- Single-driven assignments
  elc <= elc;
  
  -- Multi-driven assignments
  bwipsa <= 'X';
  bwipsa <= 'U';
  bwipsa <= bwipsa;
  bwipsa <= bwipsa;
end pk;

entity c is
  port (tqbisstv : buffer bit_vector(0 downto 4));
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture nq of c is
  signal vutfd : time;
  signal ki : std_logic;
  signal gpdzxwfbf : time;
  signal kapdfr : std_logic;
  signal njjz : time;
  signal uhhxyax : std_logic;
begin
  eanlrbq : entity work.kyvp
    port map (bwipsa => uhhxyax, elc => njjz);
  vdzct : entity work.kyvp
    port map (bwipsa => kapdfr, elc => gpdzxwfbf);
  qejlldvqzm : entity work.kyvp
    port map (bwipsa => ki, elc => vutfd);
  
  -- Single-driven assignments
  tqbisstv <= (others => '0');
  
  -- Multi-driven assignments
  uhhxyax <= 'H';
  kapdfr <= uhhxyax;
  uhhxyax <= '1';
  ki <= ki;
end nq;

library ieee;
use ieee.std_logic_1164.all;

entity uouford is
  port (eyphudfdv : in std_logic; hpa : out integer; fvcnnawfb : buffer time; aw : buffer boolean_vector(3 to 3));
end uouford;

architecture cxfydrtmff of uouford is
  signal z : bit_vector(0 downto 4);
begin
  xwzkexj : entity work.c
    port map (tqbisstv => z);
  
  -- Single-driven assignments
  aw <= aw;
  fvcnnawfb <= fvcnnawfb;
end cxfydrtmff;

library ieee;
use ieee.std_logic_1164.all;

entity vsaltqh is
  port (anungn : inout time_vector(1 downto 3); ub : linkage std_logic; aqzquoi : in std_logic_vector(0 to 4); wn : inout real);
end vsaltqh;

architecture nprlfqlsdx of vsaltqh is
  
begin
  -- Single-driven assignments
  wn <= 1324.0;
end nprlfqlsdx;



-- Seed after: 553698909439518299,18037650846010261179
