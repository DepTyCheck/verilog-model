-- Seed: 4580241282279836223,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity vquuu is
  port ( heovyv : linkage std_logic_vector(4 to 1)
  ; ft : out std_logic_vector(4 to 0)
  ; xdukiumfn : out real_vector(2 downto 3)
  ; qqlpdyekc : linkage std_logic
  );
end vquuu;

architecture tb of vquuu is
  
begin
  -- Single-driven assignments
  xdukiumfn <= (others => 0.0);
  
  -- Multi-driven assignments
  ft <= (others => '0');
  ft <= ft;
  ft <= "";
  ft <= "";
end tb;

library ieee;
use ieee.std_logic_1164.all;

entity lo is
  port (fzzalc : linkage std_logic; qcxfmc : inout std_logic_vector(1 downto 2));
end lo;

library ieee;
use ieee.std_logic_1164.all;

architecture tcgua of lo is
  signal zzc : std_logic;
  signal oitd : real_vector(2 downto 3);
  signal fi : std_logic_vector(4 to 0);
  signal fqr : std_logic_vector(4 to 1);
  signal y : std_logic;
  signal fmo : real_vector(2 downto 3);
  signal tjdclh : std_logic;
  signal aioyexjdat : real_vector(2 downto 3);
  signal fjiuotuzs : std_logic_vector(4 to 1);
  signal eqirznjdne : real_vector(2 downto 3);
  signal ezgagcovwh : std_logic_vector(4 to 0);
begin
  wjuqz : entity work.vquuu
    port map (heovyv => qcxfmc, ft => ezgagcovwh, xdukiumfn => eqirznjdne, qqlpdyekc => fzzalc);
  ayfiu : entity work.vquuu
    port map (heovyv => qcxfmc, ft => fjiuotuzs, xdukiumfn => aioyexjdat, qqlpdyekc => tjdclh);
  fnv : entity work.vquuu
    port map (heovyv => fjiuotuzs, ft => qcxfmc, xdukiumfn => fmo, qqlpdyekc => y);
  tdohmeoezz : entity work.vquuu
    port map (heovyv => fqr, ft => fi, xdukiumfn => oitd, qqlpdyekc => zzc);
  
  -- Multi-driven assignments
  qcxfmc <= (others => '0');
  y <= '-';
end tcgua;

library ieee;
use ieee.std_logic_1164.all;

entity xxocmple is
  port (qybdbh : out std_logic_vector(0 to 0); epkmkyoa : in std_logic_vector(0 downto 0); uwzwluucu : out integer);
end xxocmple;

library ieee;
use ieee.std_logic_1164.all;

architecture omtk of xxocmple is
  signal nczqsleu : real_vector(2 downto 3);
  signal htwdtyihxu : std_logic_vector(4 to 0);
  signal mvuwlh : std_logic;
  signal nlrwmpet : real_vector(2 downto 3);
  signal nsenuj : std_logic_vector(1 downto 2);
  signal mku : std_logic;
  signal opejyvq : real_vector(2 downto 3);
  signal pfmiblqmi : std_logic_vector(4 to 0);
  signal smmceiboj : std_logic_vector(4 to 1);
begin
  mvfyqfacpc : entity work.vquuu
    port map (heovyv => smmceiboj, ft => pfmiblqmi, xdukiumfn => opejyvq, qqlpdyekc => mku);
  pxjlecyq : entity work.vquuu
    port map (heovyv => smmceiboj, ft => nsenuj, xdukiumfn => nlrwmpet, qqlpdyekc => mvuwlh);
  pmdwwbl : entity work.vquuu
    port map (heovyv => smmceiboj, ft => htwdtyihxu, xdukiumfn => nczqsleu, qqlpdyekc => mku);
  svtbhiggd : entity work.lo
    port map (fzzalc => mku, qcxfmc => nsenuj);
  
  -- Single-driven assignments
  uwzwluucu <= 8#4_3#;
  
  -- Multi-driven assignments
  mku <= mku;
  pfmiblqmi <= smmceiboj;
  qybdbh <= epkmkyoa;
end omtk;

library ieee;
use ieee.std_logic_1164.all;

entity tg is
  port (jg : buffer std_logic_vector(3 downto 3); nkdkj : in real);
end tg;

library ieee;
use ieee.std_logic_1164.all;

architecture enwrkigdyt of tg is
  signal wghma : integer;
  signal l : std_logic_vector(0 downto 0);
  signal hragfwuii : std_logic;
  signal bjfyfxf : real_vector(2 downto 3);
  signal xeqnnzliw : std_logic_vector(4 to 0);
  signal jpcumba : std_logic_vector(4 to 1);
begin
  nyqld : entity work.vquuu
    port map (heovyv => jpcumba, ft => xeqnnzliw, xdukiumfn => bjfyfxf, qqlpdyekc => hragfwuii);
  hxs : entity work.xxocmple
    port map (qybdbh => jg, epkmkyoa => l, uwzwluucu => wghma);
  
  -- Multi-driven assignments
  jg <= l;
end enwrkigdyt;



-- Seed after: 14990605516825001925,3400751927341804175
