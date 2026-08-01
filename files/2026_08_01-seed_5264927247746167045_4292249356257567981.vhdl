-- Seed: 5264927247746167045,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity as is
  port (uunor : in std_logic_vector(3 downto 1); hupx : inout integer_vector(1 downto 3); wu : in time);
end as;

architecture kyjrlqojd of as is
  
begin
  -- Single-driven assignments
  hupx <= (others => 0);
end kyjrlqojd;

entity dvuqs is
  port (otfxsk : inout real);
end dvuqs;

library ieee;
use ieee.std_logic_1164.all;

architecture dkq of dvuqs is
  signal inku : time;
  signal ci : integer_vector(1 downto 3);
  signal nkhckqhzys : integer_vector(1 downto 3);
  signal mkkxkzt : time;
  signal mn : integer_vector(1 downto 3);
  signal rj : time;
  signal pygcvhoh : integer_vector(1 downto 3);
  signal qtv : std_logic_vector(3 downto 1);
begin
  tlsrjqj : entity work.as
    port map (uunor => qtv, hupx => pygcvhoh, wu => rj);
  setyi : entity work.as
    port map (uunor => qtv, hupx => mn, wu => mkkxkzt);
  eki : entity work.as
    port map (uunor => qtv, hupx => nkhckqhzys, wu => mkkxkzt);
  caj : entity work.as
    port map (uunor => qtv, hupx => ci, wu => inku);
  
  -- Single-driven assignments
  mkkxkzt <= rj;
  
  -- Multi-driven assignments
  qtv <= qtv;
  qtv <= qtv;
  qtv <= "ZW0";
  qtv <= "X0U";
end dkq;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (p : in boolean; beugc : out real; ogdkzvxaar : out std_logic_vector(1 downto 0); qvqxhia : linkage integer);
end b;

architecture bnh of b is
  
begin
  -- Single-driven assignments
  beugc <= beugc;
  
  -- Multi-driven assignments
  ogdkzvxaar <= "H0";
  ogdkzvxaar <= ('W', '-');
  ogdkzvxaar <= ('-', 'X');
  ogdkzvxaar <= "-X";
end bnh;



-- Seed after: 9142281950957455386,4292249356257567981
