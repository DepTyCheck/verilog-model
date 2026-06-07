-- Seed: 356826955802186796,13903879141658024201



entity jbzxe is
  port ( lnfdtzhqyk : out boolean_vector(1 downto 4)
  ; utsfwjjm : buffer bit_vector(0 to 3)
  ; wom : inout real_vector(3 downto 0)
  ; agadq : linkage severity_level
  );
end jbzxe;



architecture kst of jbzxe is
  
begin
  
end kst;



entity occl is
  port (fopya : in time_vector(1 to 0); cqmmmgws : in real);
end occl;



architecture pyvt of occl is
  signal fp : severity_level;
  signal uuffuuppt : real_vector(3 downto 0);
  signal tgz : bit_vector(0 to 3);
  signal fqzcjjrdl : boolean_vector(1 downto 4);
  signal vc : severity_level;
  signal yeytebhuzq : real_vector(3 downto 0);
  signal w : bit_vector(0 to 3);
  signal wcdocw : boolean_vector(1 downto 4);
begin
  qraaxbu : entity work.jbzxe
    port map (lnfdtzhqyk => wcdocw, utsfwjjm => w, wom => yeytebhuzq, agadq => vc);
  tj : entity work.jbzxe
    port map (lnfdtzhqyk => fqzcjjrdl, utsfwjjm => tgz, wom => uuffuuppt, agadq => fp);
end pyvt;

library ieee;
use ieee.std_logic_1164.all;

entity cxqvaved is
  port (bike : buffer std_logic_vector(2 downto 1));
end cxqvaved;



architecture iwhqc of cxqvaved is
  signal ammishgw : real;
  signal ev : time_vector(1 to 0);
  signal fdezs : severity_level;
  signal uefw : real_vector(3 downto 0);
  signal radgk : bit_vector(0 to 3);
  signal ckpik : boolean_vector(1 downto 4);
  signal av : severity_level;
  signal dx : real_vector(3 downto 0);
  signal igpokm : bit_vector(0 to 3);
  signal ildxyoybdl : boolean_vector(1 downto 4);
  signal nimcercd : severity_level;
  signal wnudktlppm : real_vector(3 downto 0);
  signal vj : bit_vector(0 to 3);
  signal zbstuppcz : boolean_vector(1 downto 4);
begin
  wvwota : entity work.jbzxe
    port map (lnfdtzhqyk => zbstuppcz, utsfwjjm => vj, wom => wnudktlppm, agadq => nimcercd);
  ksgumeiytv : entity work.jbzxe
    port map (lnfdtzhqyk => ildxyoybdl, utsfwjjm => igpokm, wom => dx, agadq => av);
  a : entity work.jbzxe
    port map (lnfdtzhqyk => ckpik, utsfwjjm => radgk, wom => uefw, agadq => fdezs);
  t : entity work.occl
    port map (fopya => ev, cqmmmgws => ammishgw);
end iwhqc;



-- Seed after: 17892890856164784778,13903879141658024201
