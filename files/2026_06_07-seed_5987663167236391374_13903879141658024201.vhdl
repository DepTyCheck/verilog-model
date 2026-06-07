-- Seed: 5987663167236391374,13903879141658024201

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (sggz : out std_logic; y : out std_logic; ogynzcxu : out integer; kjlxeyfjm : inout time_vector(4 downto 0));
end p;



architecture cnjhkhap of p is
  
begin
  
end cnjhkhap;

library ieee;
use ieee.std_logic_1164.all;

entity ystwnwz is
  port (yhtxhso : out std_logic);
end ystwnwz;

library ieee;
use ieee.std_logic_1164.all;

architecture u of ystwnwz is
  signal hrpcheasow : time_vector(4 downto 0);
  signal qvafwgx : integer;
  signal spg : std_logic;
  signal nltfpugxy : time_vector(4 downto 0);
  signal rmbprkc : integer;
  signal ktnesyaeav : time_vector(4 downto 0);
  signal cg : integer;
  signal t : std_logic;
  signal m : time_vector(4 downto 0);
  signal nqjseau : integer;
  signal mcyy : std_logic;
begin
  rlsyknmin : entity work.p
    port map (sggz => yhtxhso, y => mcyy, ogynzcxu => nqjseau, kjlxeyfjm => m);
  x : entity work.p
    port map (sggz => yhtxhso, y => t, ogynzcxu => cg, kjlxeyfjm => ktnesyaeav);
  exz : entity work.p
    port map (sggz => yhtxhso, y => t, ogynzcxu => rmbprkc, kjlxeyfjm => nltfpugxy);
  piw : entity work.p
    port map (sggz => spg, y => mcyy, ogynzcxu => qvafwgx, kjlxeyfjm => hrpcheasow);
end u;



-- Seed after: 1125080627528902697,13903879141658024201
