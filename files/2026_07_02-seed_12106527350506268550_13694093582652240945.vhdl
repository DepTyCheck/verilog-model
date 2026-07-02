-- Seed: 12106527350506268550,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (jejaoi : inout time; znwghartx : buffer std_logic_vector(0 to 4); truvhbjcx : linkage time; cyh : out integer);
end f;

architecture paox of f is
  
begin
  -- Single-driven assignments
  jejaoi <= 1 sec;
  cyh <= 16#686#;
  
  -- Multi-driven assignments
  znwghartx <= "WLUW1";
  znwghartx <= "HWU1W";
  znwghartx <= ('W', 'W', '1', 'W', '-');
end paox;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (nrickp : inout bit; bibme : in integer; ufndy : buffer integer; nt : in std_logic_vector(3 to 4));
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture atuom of v is
  signal skci : time;
  signal yedrluc : std_logic_vector(0 to 4);
  signal vjjf : time;
  signal xltnndycua : integer;
  signal uc : time;
  signal ssznnd : time;
  signal oyonfrsew : integer;
  signal mtj : time;
  signal fzbwptsox : std_logic_vector(0 to 4);
  signal kvnyifmjf : time;
begin
  pnpk : entity work.f
    port map (jejaoi => kvnyifmjf, znwghartx => fzbwptsox, truvhbjcx => mtj, cyh => oyonfrsew);
  je : entity work.f
    port map (jejaoi => ssznnd, znwghartx => fzbwptsox, truvhbjcx => uc, cyh => xltnndycua);
  wnaf : entity work.f
    port map (jejaoi => vjjf, znwghartx => yedrluc, truvhbjcx => skci, cyh => ufndy);
  
  -- Single-driven assignments
  nrickp <= '1';
  
  -- Multi-driven assignments
  yedrluc <= "0UUWL";
  fzbwptsox <= "H-XX-";
  yedrluc <= "H--11";
end atuom;



-- Seed after: 1092728532018468072,13694093582652240945
