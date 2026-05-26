-- Seed: 7749033062732486332,8089241273282434469

library ieee;
use ieee.std_logic_1164.all;

entity msbkyd is
  port (nlhrttyi : inout std_logic_vector(0 downto 2); xorqctah : in std_logic);
end msbkyd;



architecture bppktnkgs of msbkyd is
  
begin
  
end bppktnkgs;



entity hgu is
  port (jxp : in real_vector(3 to 1); jhifv : out integer);
end hgu;

library ieee;
use ieee.std_logic_1164.all;

architecture tzcp of hgu is
  signal du : std_logic;
  signal kux : std_logic_vector(0 downto 2);
begin
  xbtre : entity work.msbkyd
    port map (nlhrttyi => kux, xorqctah => du);
end tzcp;



entity xieoq is
  port (xlaawzxvfb : inout boolean; whixpw : buffer time);
end xieoq;



architecture e of xieoq is
  signal cdf : integer;
  signal rqtxfhracd : real_vector(3 to 1);
begin
  ywmcx : entity work.hgu
    port map (jxp => rqtxfhracd, jhifv => cdf);
end e;

library ieee;
use ieee.std_logic_1164.all;

entity dzbcdxow is
  port (oa : buffer integer; jhqkexm : buffer boolean_vector(0 to 0); teh : inout std_logic_vector(1 to 2); od : linkage std_logic_vector(3 to 3));
end dzbcdxow;

library ieee;
use ieee.std_logic_1164.all;

architecture qvvccoeqmg of dzbcdxow is
  signal sjxz : std_logic;
  signal yuven : std_logic_vector(0 downto 2);
  signal covkynb : real_vector(3 to 1);
  signal seqzxyd : integer;
  signal tvklibv : integer;
  signal ndwa : real_vector(3 to 1);
begin
  yggwhf : entity work.hgu
    port map (jxp => ndwa, jhifv => tvklibv);
  cct : entity work.hgu
    port map (jxp => ndwa, jhifv => seqzxyd);
  xyege : entity work.hgu
    port map (jxp => covkynb, jhifv => oa);
  pshzr : entity work.msbkyd
    port map (nlhrttyi => yuven, xorqctah => sjxz);
end qvvccoeqmg;



-- Seed after: 412571656370165256,8089241273282434469
