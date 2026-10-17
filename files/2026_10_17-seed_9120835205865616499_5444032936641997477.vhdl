-- Seed: 9120835205865616499,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity tejkxe is
  port (mwskezc : buffer boolean_vector(4 downto 3); hfsgbttx : out std_logic);
end tejkxe;

architecture ozkpnhtnq of tejkxe is
  
begin
  -- Single-driven assignments
  mwskezc <= mwskezc;
end ozkpnhtnq;

entity ooalz is
  port (sxlqq : in time);
end ooalz;

library ieee;
use ieee.std_logic_1164.all;

architecture pjft of ooalz is
  signal wci : std_logic;
  signal rnznbdyfr : boolean_vector(4 downto 3);
  signal opfbougyhs : std_logic;
  signal t : boolean_vector(4 downto 3);
  signal buyduz : std_logic;
  signal fhjwxptm : boolean_vector(4 downto 3);
begin
  vksewuud : entity work.tejkxe
    port map (mwskezc => fhjwxptm, hfsgbttx => buyduz);
  shzsjl : entity work.tejkxe
    port map (mwskezc => t, hfsgbttx => opfbougyhs);
  wcfi : entity work.tejkxe
    port map (mwskezc => rnznbdyfr, hfsgbttx => wci);
  
  -- Multi-driven assignments
  buyduz <= opfbougyhs;
  wci <= buyduz;
  buyduz <= buyduz;
end pjft;

entity zwmv is
  port (adbfr : out time);
end zwmv;

library ieee;
use ieee.std_logic_1164.all;

architecture pgqmrv of zwmv is
  signal ixogjodwx : time;
  signal qpwcsxeo : std_logic;
  signal nyatpfnfje : boolean_vector(4 downto 3);
  signal rnq : std_logic;
  signal fnagdiab : boolean_vector(4 downto 3);
begin
  qcnl : entity work.tejkxe
    port map (mwskezc => fnagdiab, hfsgbttx => rnq);
  tlaf : entity work.tejkxe
    port map (mwskezc => nyatpfnfje, hfsgbttx => qpwcsxeo);
  xmxiqjr : entity work.ooalz
    port map (sxlqq => ixogjodwx);
  
  -- Single-driven assignments
  adbfr <= adbfr;
  ixogjodwx <= adbfr;
  
  -- Multi-driven assignments
  rnq <= 'Z';
  rnq <= 'X';
  rnq <= rnq;
  rnq <= rnq;
end pgqmrv;



-- Seed after: 8725975304194580937,5444032936641997477
