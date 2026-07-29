-- Seed: 6167842367377202720,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity iwjchrrhpq is
  port (dopau : out boolean_vector(0 downto 0); d : buffer std_logic);
end iwjchrrhpq;

architecture ioozvquu of iwjchrrhpq is
  
begin
  -- Single-driven assignments
  dopau <= dopau;
  
  -- Multi-driven assignments
  d <= d;
  d <= d;
  d <= 'H';
end ioozvquu;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (jrk : out integer_vector(3 downto 3); vuzmlpdpk : out std_logic);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture trms of e is
  signal yi : std_logic;
  signal cjiw : boolean_vector(0 downto 0);
  signal yp : std_logic;
  signal bfx : boolean_vector(0 downto 0);
  signal ycbha : boolean_vector(0 downto 0);
  signal obr : std_logic;
  signal x : boolean_vector(0 downto 0);
begin
  xwylcqmx : entity work.iwjchrrhpq
    port map (dopau => x, d => obr);
  fsraw : entity work.iwjchrrhpq
    port map (dopau => ycbha, d => vuzmlpdpk);
  lxpvia : entity work.iwjchrrhpq
    port map (dopau => bfx, d => yp);
  eezvercljh : entity work.iwjchrrhpq
    port map (dopau => cjiw, d => yi);
  
  -- Single-driven assignments
  jrk <= (others => 1_3_2);
  
  -- Multi-driven assignments
  yi <= vuzmlpdpk;
  obr <= yi;
  obr <= vuzmlpdpk;
  yi <= '0';
end trms;

library ieee;
use ieee.std_logic_1164.all;

entity ayxse is
  port (ncuaup : buffer time; uopzytemgl : inout bit_vector(0 to 0); jixiosfo : out std_logic_vector(3 to 3); pnjccy : inout time_vector(4 downto 1));
end ayxse;

library ieee;
use ieee.std_logic_1164.all;

architecture iwtylhjwu of ayxse is
  signal ci : integer_vector(3 downto 3);
  signal vphl : std_logic;
  signal ycda : boolean_vector(0 downto 0);
  signal wljypt : std_logic;
  signal mkcimom : boolean_vector(0 downto 0);
begin
  ebg : entity work.iwjchrrhpq
    port map (dopau => mkcimom, d => wljypt);
  bfijon : entity work.iwjchrrhpq
    port map (dopau => ycda, d => vphl);
  jsuzv : entity work.e
    port map (jrk => ci, vuzmlpdpk => wljypt);
  
  -- Single-driven assignments
  pnjccy <= (8#37.0_0_1_3_1# us, 3_1_4_4.2 ms, 8#3_3_0_1.5_2_2_6_0# us, 1_4_4_1_2 ns);
  ncuaup <= 2#10# us;
  uopzytemgl <= (others => '0');
end iwtylhjwu;



-- Seed after: 335786404792363074,14641901754878719179
