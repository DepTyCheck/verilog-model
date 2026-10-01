-- Seed: 9040626806857803643,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity inzpj is
  port (senoh : out time; zpcf : linkage std_logic; ks : linkage string(1 to 2); u : buffer real);
end inzpj;

architecture gwipfqu of inzpj is
  
begin
  -- Single-driven assignments
  u <= 2#0_1.0_1_1_1_1#;
  senoh <= 1 ns;
end gwipfqu;

entity yjnoc is
  port (pcczgze : buffer integer; entpxkhnds : linkage time; psu : in time);
end yjnoc;

library ieee;
use ieee.std_logic_1164.all;

architecture trdzjpj of yjnoc is
  signal odzeezzcd : real;
  signal sgtplw : string(1 to 2);
  signal uvdwair : time;
  signal v : real;
  signal te : string(1 to 2);
  signal qwgmfgt : std_logic;
  signal uscfjwkb : time;
  signal mwsn : real;
  signal nfokmkue : string(1 to 2);
  signal hhoxzi : std_logic;
  signal ac : time;
begin
  buqzskm : entity work.inzpj
    port map (senoh => ac, zpcf => hhoxzi, ks => nfokmkue, u => mwsn);
  lcyiaekus : entity work.inzpj
    port map (senoh => uscfjwkb, zpcf => qwgmfgt, ks => te, u => v);
  defqvzxx : entity work.inzpj
    port map (senoh => uvdwair, zpcf => hhoxzi, ks => sgtplw, u => odzeezzcd);
  
  -- Single-driven assignments
  pcczgze <= 16#C_1_6_2_6#;
end trdzjpj;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (mwcmz : out std_logic; plm : inout time);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture skxvhw of o is
  signal uhfjfcgayo : real;
  signal mqiugs : string(1 to 2);
  signal hkjdl : std_logic;
  signal v : time;
begin
  hf : entity work.inzpj
    port map (senoh => v, zpcf => hkjdl, ks => mqiugs, u => uhfjfcgayo);
  
  -- Single-driven assignments
  plm <= plm;
  
  -- Multi-driven assignments
  mwcmz <= 'X';
  mwcmz <= 'W';
end skxvhw;



-- Seed after: 2627937084285037929,15025465285671019065
