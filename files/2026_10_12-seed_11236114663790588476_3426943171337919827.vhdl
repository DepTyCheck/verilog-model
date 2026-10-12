-- Seed: 11236114663790588476,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity mzfuaxqqo is
  port ( awxbbia : out std_logic_vector(0 to 3)
  ; lbrlra : out std_logic_vector(4 to 2)
  ; rnuc : linkage real
  ; ffdkyccjkm : linkage std_logic_vector(1 downto 3)
  );
end mzfuaxqqo;

architecture ztjyfj of mzfuaxqqo is
  
begin
  -- Multi-driven assignments
  lbrlra <= lbrlra;
  lbrlra <= (others => '0');
  lbrlra <= (others => '0');
end ztjyfj;

entity ucdfzch is
  port (yjxs : linkage time_vector(2 downto 4));
end ucdfzch;

library ieee;
use ieee.std_logic_1164.all;

architecture geeq of ucdfzch is
  signal eofuxb : real;
  signal nfttmgnllu : std_logic_vector(4 to 2);
  signal wufrqvkkdd : std_logic_vector(1 downto 3);
  signal aganhtkzh : real;
  signal uubkl : std_logic_vector(1 downto 3);
  signal lt : real;
  signal c : std_logic_vector(1 downto 3);
  signal mscpuwvb : std_logic_vector(0 to 3);
begin
  ovbosmfoff : entity work.mzfuaxqqo
    port map (awxbbia => mscpuwvb, lbrlra => c, rnuc => lt, ffdkyccjkm => uubkl);
  wpyxjgi : entity work.mzfuaxqqo
    port map (awxbbia => mscpuwvb, lbrlra => c, rnuc => aganhtkzh, ffdkyccjkm => wufrqvkkdd);
  wclzviw : entity work.mzfuaxqqo
    port map (awxbbia => mscpuwvb, lbrlra => nfttmgnllu, rnuc => eofuxb, ffdkyccjkm => c);
  
  -- Multi-driven assignments
  nfttmgnllu <= c;
  uubkl <= c;
  mscpuwvb <= ('-', 'L', 'W', '-');
  c <= "";
end geeq;

entity wq is
  port (bfg : buffer real; cvpjaqfxvy : linkage real);
end wq;

library ieee;
use ieee.std_logic_1164.all;

architecture xyebk of wq is
  signal ykbxcgwq : std_logic_vector(4 to 2);
  signal q : real;
  signal pmetyq : real;
  signal wwxwcbi : std_logic_vector(1 downto 3);
  signal ftmdoi : std_logic_vector(0 to 3);
  signal ouso : time_vector(2 downto 4);
begin
  soplngxcmc : entity work.ucdfzch
    port map (yjxs => ouso);
  eeprbj : entity work.mzfuaxqqo
    port map (awxbbia => ftmdoi, lbrlra => wwxwcbi, rnuc => pmetyq, ffdkyccjkm => wwxwcbi);
  nne : entity work.mzfuaxqqo
    port map (awxbbia => ftmdoi, lbrlra => wwxwcbi, rnuc => q, ffdkyccjkm => ykbxcgwq);
  gtd : entity work.mzfuaxqqo
    port map (awxbbia => ftmdoi, lbrlra => ykbxcgwq, rnuc => bfg, ffdkyccjkm => wwxwcbi);
  
  -- Multi-driven assignments
  ftmdoi <= ftmdoi;
end xyebk;



-- Seed after: 10983452682546701423,3426943171337919827
