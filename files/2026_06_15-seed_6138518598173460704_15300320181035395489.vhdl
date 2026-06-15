-- Seed: 6138518598173460704,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity ghu is
  port (yjrmuquc : in real; qgndnoc : out time; thbu : inout std_logic);
end ghu;

architecture mdakpkwy of ghu is
  
begin
  -- Single-driven assignments
  qgndnoc <= 031 ps;
  
  -- Multi-driven assignments
  thbu <= 'X';
end mdakpkwy;

entity mujqshhx is
  port (i : inout real);
end mujqshhx;

library ieee;
use ieee.std_logic_1164.all;

architecture tslx of mujqshhx is
  signal nszdkcqqz : time;
  signal bwfpwrwivb : real;
  signal wmjrxhnxf : std_logic;
  signal kbiyaigk : time;
  signal bgboypkq : time;
  signal oqrl : real;
  signal t : std_logic;
  signal zpwwaiphvw : time;
begin
  ilxjf : entity work.ghu
    port map (yjrmuquc => i, qgndnoc => zpwwaiphvw, thbu => t);
  zaj : entity work.ghu
    port map (yjrmuquc => oqrl, qgndnoc => bgboypkq, thbu => t);
  uwqb : entity work.ghu
    port map (yjrmuquc => i, qgndnoc => kbiyaigk, thbu => wmjrxhnxf);
  yodqencrks : entity work.ghu
    port map (yjrmuquc => bwfpwrwivb, qgndnoc => nszdkcqqz, thbu => wmjrxhnxf);
  
  -- Single-driven assignments
  bwfpwrwivb <= 2#1_1.0_1#;
  oqrl <= 8#1_5.473#;
  i <= 16#A_A_5_E_F.0_8_B_1#;
  
  -- Multi-driven assignments
  t <= 'L';
  t <= '1';
  t <= 'X';
  t <= 'U';
end tslx;

library ieee;
use ieee.std_logic_1164.all;

entity tecq is
  port (lupapjifg : buffer std_logic_vector(0 downto 3));
end tecq;

library ieee;
use ieee.std_logic_1164.all;

architecture rfrzmiewb of tecq is
  signal eaw : time;
  signal ibv : time;
  signal zqkgksngja : real;
  signal zmmasf : time;
  signal xbwolhm : real;
  signal sy : std_logic;
  signal hejdd : time;
  signal ddwn : real;
begin
  inogyw : entity work.ghu
    port map (yjrmuquc => ddwn, qgndnoc => hejdd, thbu => sy);
  kgrdcti : entity work.ghu
    port map (yjrmuquc => xbwolhm, qgndnoc => zmmasf, thbu => sy);
  lrbjgvn : entity work.ghu
    port map (yjrmuquc => zqkgksngja, qgndnoc => ibv, thbu => sy);
  gyycjgek : entity work.ghu
    port map (yjrmuquc => xbwolhm, qgndnoc => eaw, thbu => sy);
  
  -- Multi-driven assignments
  sy <= 'W';
  lupapjifg <= "";
  lupapjifg <= (others => '0');
end rfrzmiewb;



-- Seed after: 976710917154095196,15300320181035395489
