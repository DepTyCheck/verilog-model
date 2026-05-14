-- Seed: 10084970378156508615,13684241126228714147



entity aivoieur is
  port (uxgdsqsutw : inout time);
end aivoieur;



architecture flkkka of aivoieur is
  
begin
  
end flkkka;



entity qm is
  port (dvqud : linkage severity_level; nubwf : inout time);
end qm;



architecture hlymhxhflp of qm is
  signal xzzpqbi : time;
  signal yxeotjx : time;
  signal rqghu : time;
begin
  vpayzrgo : entity work.aivoieur
    port map (uxgdsqsutw => rqghu);
  uljxbfu : entity work.aivoieur
    port map (uxgdsqsutw => yxeotjx);
  hym : entity work.aivoieur
    port map (uxgdsqsutw => xzzpqbi);
  vk : entity work.aivoieur
    port map (uxgdsqsutw => nubwf);
end hlymhxhflp;



entity cftlmdlsau is
  port (pchf : linkage real; vtlgj : inout time);
end cftlmdlsau;



architecture tihxhpbgcr of cftlmdlsau is
  signal qv : time;
  signal trhiifcuyt : time;
  signal zvajhnljs : time;
begin
  w : entity work.aivoieur
    port map (uxgdsqsutw => zvajhnljs);
  pybbcaire : entity work.aivoieur
    port map (uxgdsqsutw => trhiifcuyt);
  vc : entity work.aivoieur
    port map (uxgdsqsutw => vtlgj);
  a : entity work.aivoieur
    port map (uxgdsqsutw => qv);
end tihxhpbgcr;

library ieee;
use ieee.std_logic_1164.all;

entity sghdq is
  port (xswaykzh : in std_logic; scdemhsj : buffer real);
end sghdq;



architecture mfdllk of sghdq is
  signal qmgli : time;
begin
  ip : entity work.cftlmdlsau
    port map (pchf => scdemhsj, vtlgj => qmgli);
end mfdllk;



-- Seed after: 5493776801711398742,13684241126228714147
