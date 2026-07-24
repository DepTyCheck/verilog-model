-- Seed: 1793944464516819636,16461708287571398341

entity dvyigdwyqe is
  port (c : inout time);
end dvyigdwyqe;

architecture vm of dvyigdwyqe is
  
begin
  -- Single-driven assignments
  c <= 4 sec;
end vm;

library ieee;
use ieee.std_logic_1164.all;

entity nuppiu is
  port (ozqrgyp : in integer; ikqv : out std_logic; ehlf : out std_logic_vector(1 downto 3); yhz : out std_logic_vector(3 to 0));
end nuppiu;

architecture dnvrb of nuppiu is
  signal eyj : time;
  signal hitt : time;
  signal sehvja : time;
  signal becqk : time;
begin
  qvfumox : entity work.dvyigdwyqe
    port map (c => becqk);
  ketl : entity work.dvyigdwyqe
    port map (c => sehvja);
  nvurggzfpb : entity work.dvyigdwyqe
    port map (c => hitt);
  jybemihhuj : entity work.dvyigdwyqe
    port map (c => eyj);
  
  -- Multi-driven assignments
  yhz <= ehlf;
  yhz <= "";
  yhz <= (others => '0');
end dnvrb;



-- Seed after: 13583236964937228785,16461708287571398341
