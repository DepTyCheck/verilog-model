-- Seed: 3359118810036372900,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity gye is
  port (ikkddrkrkp : linkage integer_vector(1 downto 2); lpqk : out time; yigmzmkw : inout std_logic_vector(4 downto 4); u : in severity_level);
end gye;

architecture jvzjlulwcp of gye is
  
begin
  -- Single-driven assignments
  lpqk <= 8#4_3_3_2_1# ms;
  
  -- Multi-driven assignments
  yigmzmkw <= "H";
  yigmzmkw <= "1";
end jvzjlulwcp;

library ieee;
use ieee.std_logic_1164.all;

entity pwzsejcr is
  port (nuqkzdteww : out integer; kpp : out bit; fzbhzhuwo : out std_logic_vector(0 to 0));
end pwzsejcr;

library ieee;
use ieee.std_logic_1164.all;

architecture hrunu of pwzsejcr is
  signal atxpdoevw : time;
  signal rqdjo : integer_vector(1 downto 2);
  signal dwzbvwzyvq : severity_level;
  signal hzfw : std_logic_vector(4 downto 4);
  signal yamas : time;
  signal aoenhrxg : integer_vector(1 downto 2);
begin
  jz : entity work.gye
    port map (ikkddrkrkp => aoenhrxg, lpqk => yamas, yigmzmkw => hzfw, u => dwzbvwzyvq);
  ikcfzrae : entity work.gye
    port map (ikkddrkrkp => rqdjo, lpqk => atxpdoevw, yigmzmkw => hzfw, u => dwzbvwzyvq);
  
  -- Single-driven assignments
  dwzbvwzyvq <= FAILURE;
  kpp <= '1';
  
  -- Multi-driven assignments
  fzbhzhuwo <= "L";
end hrunu;

entity lfhwwrsg is
  port (m : inout time; wpjh : buffer real);
end lfhwwrsg;

library ieee;
use ieee.std_logic_1164.all;

architecture sbvi of lfhwwrsg is
  signal mptfrp : std_logic_vector(0 to 0);
  signal yf : bit;
  signal jqkgdvisgg : integer;
  signal amesrnmv : severity_level;
  signal hidfigh : std_logic_vector(4 downto 4);
  signal djspsbgpu : time;
  signal lkjt : integer_vector(1 downto 2);
begin
  wemxa : entity work.gye
    port map (ikkddrkrkp => lkjt, lpqk => djspsbgpu, yigmzmkw => hidfigh, u => amesrnmv);
  zi : entity work.pwzsejcr
    port map (nuqkzdteww => jqkgdvisgg, kpp => yf, fzbhzhuwo => mptfrp);
  
  -- Single-driven assignments
  m <= 4_2_1 fs;
  amesrnmv <= WARNING;
  wpjh <= 16#32472.4#;
end sbvi;

library ieee;
use ieee.std_logic_1164.all;

entity zpikrblc is
  port (zjsm : out severity_level; vkdbmg : in string(3 to 1); yqspp : in std_logic_vector(2 to 3));
end zpikrblc;

library ieee;
use ieee.std_logic_1164.all;

architecture rtae of zpikrblc is
  signal e : severity_level;
  signal ygqhonjj : std_logic_vector(4 downto 4);
  signal ad : time;
  signal ub : integer_vector(1 downto 2);
  signal fnod : std_logic_vector(0 to 0);
  signal fn : bit;
  signal yaiisaqy : integer;
begin
  xq : entity work.pwzsejcr
    port map (nuqkzdteww => yaiisaqy, kpp => fn, fzbhzhuwo => fnod);
  rrnxekqotf : entity work.gye
    port map (ikkddrkrkp => ub, lpqk => ad, yigmzmkw => ygqhonjj, u => e);
  
  -- Single-driven assignments
  zjsm <= NOTE;
  e <= WARNING;
  
  -- Multi-driven assignments
  fnod <= "-";
  fnod <= (others => 'Z');
end rtae;



-- Seed after: 10611901898655923795,6697892553037813751
