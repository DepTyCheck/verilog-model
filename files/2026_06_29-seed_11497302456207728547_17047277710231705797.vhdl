-- Seed: 11497302456207728547,17047277710231705797

entity wxkaaonoz is
  port (asmbmliil : inout real; ybsdf : in boolean; nvdxn : linkage bit; uhzrbsxf : in real);
end wxkaaonoz;

architecture ewlzsjhfi of wxkaaonoz is
  
begin
  -- Single-driven assignments
  asmbmliil <= 4_2_1_0_1.01300;
end ewlzsjhfi;

library ieee;
use ieee.std_logic_1164.all;

entity hrtmd is
  port (zqrujkb : in std_logic; rlwnvfqv : in std_logic; msi : out severity_level);
end hrtmd;

architecture oxn of hrtmd is
  signal htgae : real;
  signal bnic : bit;
  signal clfyxichok : bit;
  signal gqyuormiuw : real;
  signal cjechs : real;
  signal wqviepbhh : bit;
  signal zmybzm : boolean;
  signal vwtu : real;
begin
  dntic : entity work.wxkaaonoz
    port map (asmbmliil => vwtu, ybsdf => zmybzm, nvdxn => wqviepbhh, uhzrbsxf => cjechs);
  naosoc : entity work.wxkaaonoz
    port map (asmbmliil => gqyuormiuw, ybsdf => zmybzm, nvdxn => clfyxichok, uhzrbsxf => cjechs);
  bfqjuk : entity work.wxkaaonoz
    port map (asmbmliil => cjechs, ybsdf => zmybzm, nvdxn => bnic, uhzrbsxf => htgae);
  
  -- Single-driven assignments
  msi <= WARNING;
  zmybzm <= TRUE;
end oxn;

library ieee;
use ieee.std_logic_1164.all;

entity nj is
  port (ma : inout severity_level; aaw : linkage std_logic_vector(3 to 2));
end nj;

library ieee;
use ieee.std_logic_1164.all;

architecture ihdfdfusd of nj is
  signal vmvzbfv : real;
  signal wnsycqdm : bit;
  signal w : boolean;
  signal tufepq : std_logic;
  signal npq : std_logic;
  signal mxnckvprvd : bit;
  signal iirnuvy : real;
  signal ezihwk : real;
  signal qdpqefo : bit;
  signal vjmk : boolean;
  signal kmxyfip : real;
begin
  ybad : entity work.wxkaaonoz
    port map (asmbmliil => kmxyfip, ybsdf => vjmk, nvdxn => qdpqefo, uhzrbsxf => ezihwk);
  lkmvqwr : entity work.wxkaaonoz
    port map (asmbmliil => iirnuvy, ybsdf => vjmk, nvdxn => mxnckvprvd, uhzrbsxf => kmxyfip);
  stw : entity work.hrtmd
    port map (zqrujkb => npq, rlwnvfqv => tufepq, msi => ma);
  titckhm : entity work.wxkaaonoz
    port map (asmbmliil => ezihwk, ybsdf => w, nvdxn => wnsycqdm, uhzrbsxf => vmvzbfv);
  
  -- Single-driven assignments
  w <= FALSE;
  vjmk <= FALSE;
  
  -- Multi-driven assignments
  tufepq <= '-';
end ihdfdfusd;

library ieee;
use ieee.std_logic_1164.all;

entity oc is
  port (arhlgje : out std_logic_vector(2 downto 2); dkojknjw : out character; oeklftex : in time; jlb : out severity_level);
end oc;

architecture kjnzlhc of oc is
  signal hqeh : bit;
  signal qacrjchttt : real;
  signal gs : bit;
  signal ozil : boolean;
  signal qahkpuh : real;
begin
  mkghqcis : entity work.wxkaaonoz
    port map (asmbmliil => qahkpuh, ybsdf => ozil, nvdxn => gs, uhzrbsxf => qahkpuh);
  ty : entity work.wxkaaonoz
    port map (asmbmliil => qacrjchttt, ybsdf => ozil, nvdxn => hqeh, uhzrbsxf => qacrjchttt);
  
  -- Multi-driven assignments
  arhlgje <= "L";
  arhlgje <= (others => 'U');
  arhlgje <= (others => 'X');
  arhlgje <= "-";
end kjnzlhc;



-- Seed after: 17582797233458475311,17047277710231705797
