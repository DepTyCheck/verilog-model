-- Seed: 5784136206725258637,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity sqq is
  port (ohldzoknil : buffer real; v : buffer std_logic_vector(2 downto 2); mvplcvxi : out std_logic_vector(1 downto 2));
end sqq;

architecture nzx of sqq is
  
begin
  -- Multi-driven assignments
  mvplcvxi <= "";
end nzx;

library ieee;
use ieee.std_logic_1164.all;

entity jcuumrilw is
  port (xeffiazv : linkage std_logic_vector(3 downto 3); c : in time; vyir : in std_logic; w : out std_logic);
end jcuumrilw;

library ieee;
use ieee.std_logic_1164.all;

architecture gnwmytnx of jcuumrilw is
  signal pmtckldpt : real;
  signal xyyvnvnlxb : real;
  signal lgxmbezb : std_logic_vector(1 downto 2);
  signal zbccq : real;
  signal hdscbdrd : std_logic_vector(1 downto 2);
  signal gicy : std_logic_vector(2 downto 2);
  signal egunbtz : real;
begin
  zalaj : entity work.sqq
    port map (ohldzoknil => egunbtz, v => gicy, mvplcvxi => hdscbdrd);
  sjca : entity work.sqq
    port map (ohldzoknil => zbccq, v => gicy, mvplcvxi => lgxmbezb);
  n : entity work.sqq
    port map (ohldzoknil => xyyvnvnlxb, v => gicy, mvplcvxi => hdscbdrd);
  opmxu : entity work.sqq
    port map (ohldzoknil => pmtckldpt, v => gicy, mvplcvxi => hdscbdrd);
  
  -- Multi-driven assignments
  hdscbdrd <= (others => '0');
  w <= vyir;
  gicy <= gicy;
  w <= 'L';
end gnwmytnx;

library ieee;
use ieee.std_logic_1164.all;

entity nb is
  port (vbi : buffer integer; ddtvkpfylk : out std_logic_vector(3 downto 4));
end nb;

library ieee;
use ieee.std_logic_1164.all;

architecture eikspm of nb is
  signal ovoqxy : std_logic;
  signal qenc : std_logic;
  signal jlnyeiawmj : time;
  signal zpwdwmkag : std_logic_vector(1 downto 2);
  signal dcnatcc : std_logic_vector(3 downto 3);
  signal jw : real;
  signal jkrg : std_logic_vector(2 downto 2);
  signal yzn : real;
  signal r : std_logic_vector(1 downto 2);
  signal bxrxrfzn : std_logic_vector(2 downto 2);
  signal dok : real;
begin
  w : entity work.sqq
    port map (ohldzoknil => dok, v => bxrxrfzn, mvplcvxi => r);
  eyxy : entity work.sqq
    port map (ohldzoknil => yzn, v => jkrg, mvplcvxi => r);
  jqkcz : entity work.sqq
    port map (ohldzoknil => jw, v => dcnatcc, mvplcvxi => zpwdwmkag);
  qypuxbjg : entity work.jcuumrilw
    port map (xeffiazv => dcnatcc, c => jlnyeiawmj, vyir => qenc, w => ovoqxy);
  
  -- Multi-driven assignments
  zpwdwmkag <= (others => '0');
  jkrg <= (others => 'Z');
end eikspm;



-- Seed after: 9594466497571184252,4404421571376382767
