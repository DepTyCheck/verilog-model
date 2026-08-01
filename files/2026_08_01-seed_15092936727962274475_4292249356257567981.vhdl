-- Seed: 15092936727962274475,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity tcdvbrcr is
  port (k : inout time; gvp : inout std_logic_vector(1 to 0));
end tcdvbrcr;

architecture cg of tcdvbrcr is
  
begin
  -- Single-driven assignments
  k <= 34220 ps;
  
  -- Multi-driven assignments
  gvp <= gvp;
  gvp <= "";
  gvp <= gvp;
end cg;

library ieee;
use ieee.std_logic_1164.all;

entity fvudai is
  port ( ralexq : linkage std_logic
  ; vvyx : inout real_vector(0 to 4)
  ; jpialsm : in std_logic_vector(2 downto 3)
  ; ydsda : buffer time_vector(2 downto 2)
  );
end fvudai;

library ieee;
use ieee.std_logic_1164.all;

architecture mmuymi of fvudai is
  signal gxnfgs : std_logic_vector(1 to 0);
  signal v : time;
  signal cgnqvfvvxt : time;
  signal cqva : std_logic_vector(1 to 0);
  signal wewqaxwvhk : time;
  signal qmf : std_logic_vector(1 to 0);
  signal yumfbknxw : time;
begin
  flhwjee : entity work.tcdvbrcr
    port map (k => yumfbknxw, gvp => qmf);
  zoob : entity work.tcdvbrcr
    port map (k => wewqaxwvhk, gvp => cqva);
  y : entity work.tcdvbrcr
    port map (k => cgnqvfvvxt, gvp => qmf);
  lbmcwiqbuy : entity work.tcdvbrcr
    port map (k => v, gvp => gxnfgs);
  
  -- Single-driven assignments
  ydsda <= (others => 2_0_3 us);
end mmuymi;

entity ebzlonyp is
  port (cqweyeldkg : out bit_vector(0 downto 4); zlm : linkage time; yyjpxgye : in real; eowkqngty : inout bit_vector(4 downto 4));
end ebzlonyp;

library ieee;
use ieee.std_logic_1164.all;

architecture lcqegi of ebzlonyp is
  signal whjpjaql : time_vector(2 downto 2);
  signal uduvtod : std_logic_vector(2 downto 3);
  signal ayywm : real_vector(0 to 4);
  signal nij : std_logic;
begin
  cpym : entity work.fvudai
    port map (ralexq => nij, vvyx => ayywm, jpialsm => uduvtod, ydsda => whjpjaql);
  
  -- Single-driven assignments
  eowkqngty <= (others => '0');
  cqweyeldkg <= (others => '0');
  
  -- Multi-driven assignments
  nij <= 'L';
  nij <= nij;
  uduvtod <= uduvtod;
  nij <= 'U';
end lcqegi;

entity jkbsglaftw is
  port (flq : out integer; do : buffer real);
end jkbsglaftw;

library ieee;
use ieee.std_logic_1164.all;

architecture gvecbks of jkbsglaftw is
  signal wkbzleaec : bit_vector(4 downto 4);
  signal mxa : time;
  signal rip : bit_vector(0 downto 4);
  signal ztkzmompa : time;
  signal rc : std_logic_vector(1 to 0);
  signal fjn : time;
begin
  d : entity work.tcdvbrcr
    port map (k => fjn, gvp => rc);
  hcobxpgkpq : entity work.tcdvbrcr
    port map (k => ztkzmompa, gvp => rc);
  mgswu : entity work.ebzlonyp
    port map (cqweyeldkg => rip, zlm => mxa, yyjpxgye => do, eowkqngty => wkbzleaec);
  
  -- Single-driven assignments
  do <= do;
  flq <= 2#0_1#;
  
  -- Multi-driven assignments
  rc <= rc;
  rc <= rc;
  rc <= rc;
  rc <= (others => '0');
end gvecbks;



-- Seed after: 9779875812716930331,4292249356257567981
