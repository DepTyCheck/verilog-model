-- Seed: 3702010366597774925,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity bsi is
  port (j : linkage integer; yc : buffer std_logic);
end bsi;

architecture g of bsi is
  
begin
  -- Multi-driven assignments
  yc <= yc;
  yc <= 'H';
  yc <= '-';
  yc <= yc;
end g;

library ieee;
use ieee.std_logic_1164.all;

entity jrvgnipqv is
  port (lrrrxov : in real_vector(1 to 0); jlgx : inout std_logic; lunibdql : inout std_logic_vector(1 to 1); oox : out time);
end jrvgnipqv;

architecture rpmyjr of jrvgnipqv is
  
begin
  -- Single-driven assignments
  oox <= oox;
  
  -- Multi-driven assignments
  lunibdql <= "X";
  lunibdql <= lunibdql;
  lunibdql <= (others => 'Z');
end rpmyjr;

entity ik is
  port (qju : linkage time);
end ik;

library ieee;
use ieee.std_logic_1164.all;

architecture lninvqafqh of ik is
  signal zr : time;
  signal oevxiwqgo : std_logic_vector(1 to 1);
  signal shzjvjcwb : std_logic;
  signal qurezrzq : real_vector(1 to 0);
  signal swlbkvkojk : std_logic;
  signal n : integer;
  signal sles : std_logic;
  signal tdfhutoc : integer;
begin
  fzbytvuwi : entity work.bsi
    port map (j => tdfhutoc, yc => sles);
  ggdrdwkx : entity work.bsi
    port map (j => n, yc => swlbkvkojk);
  edeqnlc : entity work.jrvgnipqv
    port map (lrrrxov => qurezrzq, jlgx => shzjvjcwb, lunibdql => oevxiwqgo, oox => zr);
  
  -- Single-driven assignments
  qurezrzq <= (others => 0.0);
  
  -- Multi-driven assignments
  sles <= '0';
  swlbkvkojk <= '0';
  sles <= shzjvjcwb;
  swlbkvkojk <= 'Z';
end lninvqafqh;



-- Seed after: 956536220493174218,12260394286515585877
