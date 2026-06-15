-- Seed: 7366687611531563047,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity zd is
  port (bwodpowqb : inout std_logic; e : linkage integer; sztium : buffer integer; qcip : buffer real);
end zd;

architecture zznvni of zd is
  
begin
  -- Single-driven assignments
  qcip <= 3_4_3_1_3.312;
  sztium <= 8#3#;
end zznvni;

entity jeonbuliiv is
  port (drvhudln : out severity_level; rdf : linkage real; xpobqn : out integer);
end jeonbuliiv;

library ieee;
use ieee.std_logic_1164.all;

architecture hkriyjvxcr of jeonbuliiv is
  signal ylmdfir : real;
  signal epzbgnyvar : integer;
  signal esfmotpjq : real;
  signal pxrf : integer;
  signal cwj : integer;
  signal fdxgd : std_logic;
  signal hoczsj : real;
  signal um : integer;
  signal mjprq : integer;
  signal u : std_logic;
  signal qxvcvxhvl : real;
  signal uzwaumpr : integer;
  signal cggggz : integer;
  signal rdhndvj : std_logic;
begin
  lhuvn : entity work.zd
    port map (bwodpowqb => rdhndvj, e => cggggz, sztium => uzwaumpr, qcip => qxvcvxhvl);
  yn : entity work.zd
    port map (bwodpowqb => u, e => mjprq, sztium => um, qcip => hoczsj);
  hwic : entity work.zd
    port map (bwodpowqb => fdxgd, e => cwj, sztium => pxrf, qcip => esfmotpjq);
  amdewzotfv : entity work.zd
    port map (bwodpowqb => fdxgd, e => xpobqn, sztium => epzbgnyvar, qcip => ylmdfir);
  
  -- Single-driven assignments
  drvhudln <= WARNING;
  
  -- Multi-driven assignments
  rdhndvj <= '1';
end hkriyjvxcr;

entity cqi is
  port (btw : buffer real; uakiis : buffer real);
end cqi;

library ieee;
use ieee.std_logic_1164.all;

architecture vts of cqi is
  signal zaxozdockx : integer;
  signal ua : integer;
  signal aknsif : integer;
  signal aod : severity_level;
  signal l : real;
  signal hvmieliovh : integer;
  signal beujfmdf : integer;
  signal sbgikr : std_logic;
begin
  gqvlrl : entity work.zd
    port map (bwodpowqb => sbgikr, e => beujfmdf, sztium => hvmieliovh, qcip => l);
  soylzvypwk : entity work.jeonbuliiv
    port map (drvhudln => aod, rdf => uakiis, xpobqn => aknsif);
  tyvh : entity work.zd
    port map (bwodpowqb => sbgikr, e => ua, sztium => zaxozdockx, qcip => btw);
  
  -- Multi-driven assignments
  sbgikr <= '-';
  sbgikr <= 'U';
  sbgikr <= 'U';
  sbgikr <= '-';
end vts;



-- Seed after: 4465904945802739363,1834764876137802293
