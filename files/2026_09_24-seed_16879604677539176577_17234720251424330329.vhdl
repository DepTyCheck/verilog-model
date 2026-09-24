-- Seed: 16879604677539176577,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (cxttlh : out std_logic_vector(4 to 3); ixlpesak : in time_vector(2 to 0));
end t;

architecture avsd of t is
  
begin
  -- Multi-driven assignments
  cxttlh <= cxttlh;
  cxttlh <= (others => '0');
end avsd;

library ieee;
use ieee.std_logic_1164.all;

entity coebtrdx is
  port (bpyc : in integer_vector(1 downto 4); ativm : in time; k : in std_logic_vector(4 to 1); ibluvg : linkage std_logic);
end coebtrdx;

library ieee;
use ieee.std_logic_1164.all;

architecture qqpuxqt of coebtrdx is
  signal qlljrpexhn : std_logic_vector(4 to 3);
  signal w : time_vector(2 to 0);
  signal lrswgn : std_logic_vector(4 to 3);
begin
  doqdnjp : entity work.t
    port map (cxttlh => lrswgn, ixlpesak => w);
  bnbrm : entity work.t
    port map (cxttlh => qlljrpexhn, ixlpesak => w);
  
  -- Multi-driven assignments
  qlljrpexhn <= k;
  lrswgn <= "";
  lrswgn <= k;
end qqpuxqt;

library ieee;
use ieee.std_logic_1164.all;

entity fuq is
  port (k : in bit; zkn : in time_vector(3 downto 2); qiunjhkis : inout std_logic_vector(1 to 3));
end fuq;

architecture vdmmyg of fuq is
  
begin
  -- Multi-driven assignments
  qiunjhkis <= qiunjhkis;
  qiunjhkis <= "HUL";
  qiunjhkis <= qiunjhkis;
  qiunjhkis <= "UW1";
end vdmmyg;

library ieee;
use ieee.std_logic_1164.all;

entity zwbq is
  port (gdgawgspjs : out time; kofougjeou : out std_logic; gdspxdvtbv : linkage real);
end zwbq;

library ieee;
use ieee.std_logic_1164.all;

architecture npgvb of zwbq is
  signal iqmzj : std_logic_vector(1 to 3);
  signal v : time_vector(3 downto 2);
  signal bkszi : bit;
  signal bmyhawcmim : std_logic_vector(4 to 3);
  signal jiruttj : std_logic;
  signal zl : time;
  signal epnvoipoms : integer_vector(1 downto 4);
  signal esm : time_vector(2 to 0);
  signal hoeblkp : std_logic_vector(4 to 1);
begin
  zvxka : entity work.t
    port map (cxttlh => hoeblkp, ixlpesak => esm);
  zo : entity work.coebtrdx
    port map (bpyc => epnvoipoms, ativm => zl, k => hoeblkp, ibluvg => jiruttj);
  p : entity work.t
    port map (cxttlh => bmyhawcmim, ixlpesak => esm);
  xfhu : entity work.fuq
    port map (k => bkszi, zkn => v, qiunjhkis => iqmzj);
  
  -- Single-driven assignments
  bkszi <= '1';
  esm <= (others => 0 ns);
  gdgawgspjs <= 8#66# ns;
  
  -- Multi-driven assignments
  kofougjeou <= '-';
  jiruttj <= kofougjeou;
end npgvb;



-- Seed after: 6390705463803140397,17234720251424330329
