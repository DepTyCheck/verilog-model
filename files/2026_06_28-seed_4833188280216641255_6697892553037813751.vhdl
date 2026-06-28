-- Seed: 4833188280216641255,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity xvtn is
  port (bqt : inout real; qi : in real_vector(0 to 3); fybvdbfxmh : inout std_logic_vector(2 downto 3); udtgd : buffer real);
end xvtn;

architecture pgb of xvtn is
  
begin
  -- Single-driven assignments
  udtgd <= 16#CE767.6_4_2_3#;
  
  -- Multi-driven assignments
  fybvdbfxmh <= "";
  fybvdbfxmh <= (others => '0');
  fybvdbfxmh <= "";
end pgb;

entity kks is
  port (wvwcvtkys : buffer time; njogok : linkage bit_vector(2 downto 3); qzsgsvjrmr : out bit_vector(4 to 3));
end kks;

library ieee;
use ieee.std_logic_1164.all;

architecture ial of kks is
  signal zanh : real;
  signal ijcwnwixwo : real_vector(0 to 3);
  signal yrg : real;
  signal gbtfawix : real;
  signal nqojpv : std_logic_vector(2 downto 3);
  signal wabvrw : real_vector(0 to 3);
  signal ciqrqdq : real;
  signal iowbvrpteg : real;
  signal rbiqeu : std_logic_vector(2 downto 3);
  signal bptxjbbxuc : real_vector(0 to 3);
  signal rgetcncchc : real;
begin
  vp : entity work.xvtn
    port map (bqt => rgetcncchc, qi => bptxjbbxuc, fybvdbfxmh => rbiqeu, udtgd => iowbvrpteg);
  zponzubi : entity work.xvtn
    port map (bqt => ciqrqdq, qi => wabvrw, fybvdbfxmh => nqojpv, udtgd => gbtfawix);
  hmae : entity work.xvtn
    port map (bqt => yrg, qi => ijcwnwixwo, fybvdbfxmh => nqojpv, udtgd => zanh);
  
  -- Single-driven assignments
  qzsgsvjrmr <= (others => '0');
  
  -- Multi-driven assignments
  rbiqeu <= "";
  nqojpv <= (others => '0');
  rbiqeu <= "";
end ial;

library ieee;
use ieee.std_logic_1164.all;

entity vyctj is
  port (q : linkage std_logic; mfrsayntlr : in std_logic_vector(1 to 3));
end vyctj;

library ieee;
use ieee.std_logic_1164.all;

architecture feb of vyctj is
  signal zvqbf : real;
  signal dps : std_logic_vector(2 downto 3);
  signal mynfruww : real_vector(0 to 3);
  signal zaoylxnjc : real;
  signal ixeuza : real;
  signal lhmqmhhmyn : std_logic_vector(2 downto 3);
  signal zvavorr : real_vector(0 to 3);
  signal hasarhfm : real;
begin
  txo : entity work.xvtn
    port map (bqt => hasarhfm, qi => zvavorr, fybvdbfxmh => lhmqmhhmyn, udtgd => ixeuza);
  kztexq : entity work.xvtn
    port map (bqt => zaoylxnjc, qi => mynfruww, fybvdbfxmh => dps, udtgd => zvqbf);
  
  -- Single-driven assignments
  mynfruww <= (2#11.1100#, 2#1_1.01100#, 16#1.2_3_3#, 02111.12);
  zvavorr <= (2#0_0_0.1_1_0_0_1#, 16#2864.1A1#, 8#1.5_4_7_1_5#, 1_1.3);
  
  -- Multi-driven assignments
  lhmqmhhmyn <= (others => '0');
  lhmqmhhmyn <= (others => '0');
  lhmqmhhmyn <= (others => '0');
  lhmqmhhmyn <= "";
end feb;

library ieee;
use ieee.std_logic_1164.all;

entity pnyww is
  port (bmh : out std_logic);
end pnyww;

library ieee;
use ieee.std_logic_1164.all;

architecture sriimxjac of pnyww is
  signal lmas : bit_vector(4 to 3);
  signal n : bit_vector(2 downto 3);
  signal u : time;
  signal opeuoequla : std_logic_vector(1 to 3);
  signal pmj : bit_vector(4 to 3);
  signal pvni : bit_vector(2 downto 3);
  signal sqmr : time;
begin
  zdqbkxsli : entity work.kks
    port map (wvwcvtkys => sqmr, njogok => pvni, qzsgsvjrmr => pmj);
  nn : entity work.vyctj
    port map (q => bmh, mfrsayntlr => opeuoequla);
  dffzxm : entity work.kks
    port map (wvwcvtkys => u, njogok => n, qzsgsvjrmr => lmas);
  
  -- Multi-driven assignments
  bmh <= 'L';
end sriimxjac;



-- Seed after: 10406400267852672065,6697892553037813751
