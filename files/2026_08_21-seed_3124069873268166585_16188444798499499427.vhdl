-- Seed: 3124069873268166585,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity wrkl is
  port (q : buffer severity_level; qojvwqtzy : out std_logic; dpjknux : out std_logic_vector(2 downto 1));
end wrkl;

architecture hubhywl of wrkl is
  
begin
  -- Single-driven assignments
  q <= q;
end hubhywl;

library ieee;
use ieee.std_logic_1164.all;

entity ua is
  port (jduakcandn : out real; dyjvt : linkage string(2 to 2); muyornj : linkage std_logic; lkqip : buffer time);
end ua;

library ieee;
use ieee.std_logic_1164.all;

architecture zfeatts of ua is
  signal jxsyw : std_logic_vector(2 downto 1);
  signal kzdngjfbyn : std_logic;
  signal cdbggury : severity_level;
begin
  izyzhu : entity work.wrkl
    port map (q => cdbggury, qojvwqtzy => kzdngjfbyn, dpjknux => jxsyw);
  
  -- Multi-driven assignments
  kzdngjfbyn <= kzdngjfbyn;
  kzdngjfbyn <= 'Z';
  kzdngjfbyn <= '-';
end zfeatts;

entity nabys is
  port (ivy : buffer real; ybadxxxzn : buffer real);
end nabys;

library ieee;
use ieee.std_logic_1164.all;

architecture aij of nabys is
  signal sbersm : severity_level;
  signal gqkltevy : std_logic_vector(2 downto 1);
  signal whgaqxyu : std_logic;
  signal ulryj : severity_level;
  signal luj : time;
  signal ggekzmdaz : std_logic;
  signal bxfmvzxdeu : string(2 to 2);
  signal uatm : real;
begin
  z : entity work.ua
    port map (jduakcandn => uatm, dyjvt => bxfmvzxdeu, muyornj => ggekzmdaz, lkqip => luj);
  ndlscp : entity work.wrkl
    port map (q => ulryj, qojvwqtzy => whgaqxyu, dpjknux => gqkltevy);
  ogdoy : entity work.wrkl
    port map (q => sbersm, qojvwqtzy => whgaqxyu, dpjknux => gqkltevy);
  
  -- Single-driven assignments
  ybadxxxzn <= uatm;
  ivy <= ybadxxxzn;
  
  -- Multi-driven assignments
  ggekzmdaz <= ggekzmdaz;
end aij;

entity ko is
  port (ugp : out boolean_vector(1 to 0); veiihkm : out real);
end ko;

library ieee;
use ieee.std_logic_1164.all;

architecture lkhhpndk of ko is
  signal cmwbdppy : time;
  signal hguyqmvd : string(2 to 2);
  signal bslw : real;
  signal cfxj : real;
  signal hturtedyz : severity_level;
  signal qjcfok : std_logic_vector(2 downto 1);
  signal ozvt : std_logic;
  signal crm : severity_level;
begin
  kgnpi : entity work.wrkl
    port map (q => crm, qojvwqtzy => ozvt, dpjknux => qjcfok);
  kmglri : entity work.wrkl
    port map (q => hturtedyz, qojvwqtzy => ozvt, dpjknux => qjcfok);
  pceodbhhop : entity work.nabys
    port map (ivy => cfxj, ybadxxxzn => veiihkm);
  upqnriofv : entity work.ua
    port map (jduakcandn => bslw, dyjvt => hguyqmvd, muyornj => ozvt, lkqip => cmwbdppy);
  
  -- Single-driven assignments
  ugp <= ugp;
  
  -- Multi-driven assignments
  ozvt <= 'U';
  ozvt <= ozvt;
  ozvt <= 'L';
end lkhhpndk;



-- Seed after: 18295858113386884147,16188444798499499427
