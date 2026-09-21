-- Seed: 13778452341103474993,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity jvsmd is
  port (gdootd : linkage time; gledr : inout character; rjxm : inout real; wvwulsroiv : buffer std_logic);
end jvsmd;

architecture oasqs of jvsmd is
  
begin
  -- Multi-driven assignments
  wvwulsroiv <= 'W';
end oasqs;

entity wojd is
  port (mxdkolgwi : buffer real; cbqxd : out string(3 downto 4));
end wojd;

library ieee;
use ieee.std_logic_1164.all;

architecture zdzhbdlhl of wojd is
  signal narxkmitk : std_logic;
  signal xxki : real;
  signal zfvydqr : character;
  signal mvkgwqbl : time;
  signal wtzjjijc : std_logic;
  signal cxbducvlca : character;
  signal faih : time;
begin
  bom : entity work.jvsmd
    port map (gdootd => faih, gledr => cxbducvlca, rjxm => mxdkolgwi, wvwulsroiv => wtzjjijc);
  rt : entity work.jvsmd
    port map (gdootd => mvkgwqbl, gledr => zfvydqr, rjxm => xxki, wvwulsroiv => narxkmitk);
  
  -- Single-driven assignments
  cbqxd <= cbqxd;
  
  -- Multi-driven assignments
  wtzjjijc <= wtzjjijc;
  narxkmitk <= wtzjjijc;
end zdzhbdlhl;

entity hmujx is
  port (sbbly : inout real);
end hmujx;

library ieee;
use ieee.std_logic_1164.all;

architecture oowyovhg of hmujx is
  signal qgqiqxdkyx : string(3 downto 4);
  signal rgqcqnxjx : real;
  signal chpfp : character;
  signal wc : time;
  signal eivauk : string(3 downto 4);
  signal jb : real;
  signal bh : std_logic;
  signal nrpmwhttt : real;
  signal gittso : character;
  signal tkzusi : time;
begin
  mzrmnjok : entity work.jvsmd
    port map (gdootd => tkzusi, gledr => gittso, rjxm => nrpmwhttt, wvwulsroiv => bh);
  rgbu : entity work.wojd
    port map (mxdkolgwi => jb, cbqxd => eivauk);
  m : entity work.jvsmd
    port map (gdootd => wc, gledr => chpfp, rjxm => sbbly, wvwulsroiv => bh);
  oscutm : entity work.wojd
    port map (mxdkolgwi => rgqcqnxjx, cbqxd => qgqiqxdkyx);
  
  -- Multi-driven assignments
  bh <= '-';
  bh <= bh;
  bh <= bh;
  bh <= 'U';
end oowyovhg;



-- Seed after: 15156710706319276661,12143220691580258643
