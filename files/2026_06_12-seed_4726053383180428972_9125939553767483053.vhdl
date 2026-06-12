-- Seed: 4726053383180428972,9125939553767483053

library ieee;
use ieee.std_logic_1164.all;

entity jzikawem is
  port (rygnqq : in std_logic_vector(2 downto 4); tsj : in time_vector(4 to 2); hn : inout time; vimudtcb : linkage std_logic_vector(4 downto 2));
end jzikawem;



architecture thywbj of jzikawem is
  
begin
  
end thywbj;

library ieee;
use ieee.std_logic_1164.all;

entity hmsizp is
  port (mpqfwi : inout real; alcehiomsm : out real; o : buffer std_logic; lfjjhoi : linkage bit_vector(2 to 3));
end hmsizp;

library ieee;
use ieee.std_logic_1164.all;

architecture shenmlblb of hmsizp is
  signal zlrqzzzssw : std_logic_vector(4 downto 2);
  signal tmykykjjr : time;
  signal zlhylqv : time_vector(4 to 2);
  signal hha : std_logic_vector(2 downto 4);
begin
  sadv : entity work.jzikawem
    port map (rygnqq => hha, tsj => zlhylqv, hn => tmykykjjr, vimudtcb => zlrqzzzssw);
end shenmlblb;

library ieee;
use ieee.std_logic_1164.all;

entity lrxp is
  port (fnzjda : buffer integer; sdue : buffer time; hhdgmwm : inout std_logic; qoklh : inout time);
end lrxp;

library ieee;
use ieee.std_logic_1164.all;

architecture avz of lrxp is
  signal rkmgp : time_vector(4 to 2);
  signal awjn : std_logic_vector(4 downto 2);
  signal rxqkmmx : time_vector(4 to 2);
  signal svihtsv : std_logic_vector(2 downto 4);
  signal pgmfkm : bit_vector(2 to 3);
  signal fgjnyaixxn : real;
  signal uwbaotb : real;
begin
  gjgmrj : entity work.hmsizp
    port map (mpqfwi => uwbaotb, alcehiomsm => fgjnyaixxn, o => hhdgmwm, lfjjhoi => pgmfkm);
  rbb : entity work.jzikawem
    port map (rygnqq => svihtsv, tsj => rxqkmmx, hn => qoklh, vimudtcb => awjn);
  ejeyewnwa : entity work.jzikawem
    port map (rygnqq => svihtsv, tsj => rkmgp, hn => sdue, vimudtcb => awjn);
end avz;



-- Seed after: 131835867314554571,9125939553767483053
