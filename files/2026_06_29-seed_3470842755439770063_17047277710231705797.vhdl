-- Seed: 3470842755439770063,17047277710231705797

entity ahpkksbfhe is
  port (zvrmjybybw : out time; axwvk : linkage time);
end ahpkksbfhe;

architecture wxtvzu of ahpkksbfhe is
  
begin
  -- Single-driven assignments
  zvrmjybybw <= 41 fs;
end wxtvzu;

entity bejgnnuhb is
  port (m : in real_vector(2 downto 0); fcni : out time_vector(4 to 1));
end bejgnnuhb;

architecture aq of bejgnnuhb is
  
begin
  -- Single-driven assignments
  fcni <= (others => 0 ns);
end aq;

library ieee;
use ieee.std_logic_1164.all;

entity wfplzazh is
  port (mumuyp : buffer std_logic_vector(4 to 3));
end wfplzazh;

architecture cehch of wfplzazh is
  signal varh : time_vector(4 to 1);
  signal aorne : real_vector(2 downto 0);
begin
  smixlum : entity work.bejgnnuhb
    port map (m => aorne, fcni => varh);
  
  -- Single-driven assignments
  aorne <= (0_0_0_4.01, 8#100.5_4_0#, 2340.0);
  
  -- Multi-driven assignments
  mumuyp <= (others => '0');
  mumuyp <= "";
end cehch;

library ieee;
use ieee.std_logic_1164.all;

entity ho is
  port (eeyamyfs : inout time; qlol : buffer std_logic; m : linkage std_logic);
end ho;

library ieee;
use ieee.std_logic_1164.all;

architecture h of ho is
  signal hz : time_vector(4 to 1);
  signal vwkg : real_vector(2 downto 0);
  signal aacjuogqi : time;
  signal n : std_logic_vector(4 to 3);
  signal orfj : time;
  signal bodthowxjf : time;
begin
  warzqbryd : entity work.ahpkksbfhe
    port map (zvrmjybybw => bodthowxjf, axwvk => orfj);
  qjfcqrif : entity work.wfplzazh
    port map (mumuyp => n);
  gnqyfzbbo : entity work.ahpkksbfhe
    port map (zvrmjybybw => eeyamyfs, axwvk => aacjuogqi);
  aq : entity work.bejgnnuhb
    port map (m => vwkg, fcni => hz);
  
  -- Single-driven assignments
  vwkg <= (0.1_1_3_4_3, 2#1_0_0_0.0_0_1#, 8#6253.7_3#);
end h;



-- Seed after: 9553479945999920335,17047277710231705797
