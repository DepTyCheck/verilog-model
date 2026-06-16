-- Seed: 739378915919697237,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity ytpoqodzl is
  port (ntrkvnycbc : in std_logic; vbsld : inout boolean_vector(1 downto 3); qlf : in std_logic; jxbg : linkage time);
end ytpoqodzl;

architecture cxnhzdoxi of ytpoqodzl is
  
begin
  -- Single-driven assignments
  vbsld <= (others => TRUE);
end cxnhzdoxi;

library ieee;
use ieee.std_logic_1164.all;

entity kzrnpchde is
  port (ici : inout integer; kbcuikg : buffer std_logic_vector(0 downto 3); hqpqb : buffer std_logic_vector(4 to 2));
end kzrnpchde;

library ieee;
use ieee.std_logic_1164.all;

architecture kfikbz of kzrnpchde is
  signal xnzdaaxk : time;
  signal brwlzzsy : std_logic;
  signal leymiygl : boolean_vector(1 downto 3);
  signal ywlgzvi : std_logic;
begin
  wnnsvynbp : entity work.ytpoqodzl
    port map (ntrkvnycbc => ywlgzvi, vbsld => leymiygl, qlf => brwlzzsy, jxbg => xnzdaaxk);
  
  -- Single-driven assignments
  ici <= 8#4570#;
  
  -- Multi-driven assignments
  kbcuikg <= "";
  hqpqb <= (others => '0');
  kbcuikg <= "";
  kbcuikg <= (others => '0');
end kfikbz;

library ieee;
use ieee.std_logic_1164.all;

entity gfdcy is
  port (syldx : buffer std_logic_vector(1 downto 3));
end gfdcy;

library ieee;
use ieee.std_logic_1164.all;

architecture kzkfnzrdt of gfdcy is
  signal czrazwdkt : time;
  signal jyswlznxti : std_logic;
  signal m : boolean_vector(1 downto 3);
  signal om : std_logic;
  signal camuvzo : std_logic_vector(0 downto 3);
  signal ljortxw : integer;
  signal tllda : integer;
  signal zbf : std_logic_vector(4 to 2);
  signal a : integer;
begin
  xv : entity work.kzrnpchde
    port map (ici => a, kbcuikg => syldx, hqpqb => zbf);
  tm : entity work.kzrnpchde
    port map (ici => tllda, kbcuikg => syldx, hqpqb => syldx);
  iinrrqkune : entity work.kzrnpchde
    port map (ici => ljortxw, kbcuikg => camuvzo, hqpqb => syldx);
  f : entity work.ytpoqodzl
    port map (ntrkvnycbc => om, vbsld => m, qlf => jyswlznxti, jxbg => czrazwdkt);
end kzkfnzrdt;



-- Seed after: 13781857525289028448,5472058987609252853
