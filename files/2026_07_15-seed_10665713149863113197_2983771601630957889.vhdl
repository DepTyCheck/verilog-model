-- Seed: 10665713149863113197,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (acclmeu : inout std_logic_vector(2 to 1); bdunefbiic : linkage std_logic; sdnbndw : linkage time);
end c;

architecture whcrnp of c is
  
begin
  
end whcrnp;

use std.reflection.all;

entity naeiepi is
  port ( variable vkutqcx : inout integer_subtype_mirror_pt
  ; afrlfgz : buffer boolean_vector(4 to 2)
  ; variable uvjzzne : inout protected_value_mirror_pt
  );
end naeiepi;

library ieee;
use ieee.std_logic_1164.all;

architecture jdbtsf of naeiepi is
  signal bpcbzs : time;
  signal qjjtpmqmi : std_logic_vector(2 to 1);
  signal snoklescaf : time;
  signal n : std_logic;
  signal qm : time;
  signal zlvi : std_logic_vector(2 to 1);
  signal lqjtuji : time;
  signal eugr : std_logic;
  signal kocwkiu : std_logic_vector(2 to 1);
begin
  vtvfm : entity work.c
    port map (acclmeu => kocwkiu, bdunefbiic => eugr, sdnbndw => lqjtuji);
  lr : entity work.c
    port map (acclmeu => zlvi, bdunefbiic => eugr, sdnbndw => qm);
  xctdbybyhr : entity work.c
    port map (acclmeu => zlvi, bdunefbiic => n, sdnbndw => snoklescaf);
  e : entity work.c
    port map (acclmeu => qjjtpmqmi, bdunefbiic => n, sdnbndw => bpcbzs);
  
  -- Single-driven assignments
  afrlfgz <= (others => TRUE);
end jdbtsf;

use std.reflection.all;

entity vknnitqlja is
  port ( variable gygxiv : inout access_subtype_mirror_pt
  ; variable dswpfhrqf : inout access_subtype_mirror_pt
  ; qrppxfwh : inout severity_level
  ; variable geycfxlaxw : inout file_subtype_mirror_pt
  );
end vknnitqlja;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture psbylsmr of vknnitqlja is
  signal xmifsmgf : time;
  shared variable ae : protected_value_mirror_pt;
  signal mkvr : boolean_vector(4 to 2);
  shared variable wmnic : integer_subtype_mirror_pt;
  signal gwa : time;
  signal agzyuprz : std_logic;
  signal rjpcsyaw : std_logic_vector(2 to 1);
  shared variable cihzdqkvlu : protected_value_mirror_pt;
  signal eusrtcwqpc : boolean_vector(4 to 2);
  shared variable ogep : integer_subtype_mirror_pt;
begin
  iquk : entity work.naeiepi
    port map (vkutqcx => ogep, afrlfgz => eusrtcwqpc, uvjzzne => cihzdqkvlu);
  xlyrddiwjm : entity work.c
    port map (acclmeu => rjpcsyaw, bdunefbiic => agzyuprz, sdnbndw => gwa);
  vlgyhnsu : entity work.naeiepi
    port map (vkutqcx => wmnic, afrlfgz => mkvr, uvjzzne => ae);
  aareaikc : entity work.c
    port map (acclmeu => rjpcsyaw, bdunefbiic => agzyuprz, sdnbndw => xmifsmgf);
  
  -- Single-driven assignments
  qrppxfwh <= FAILURE;
  
  -- Multi-driven assignments
  rjpcsyaw <= rjpcsyaw;
  rjpcsyaw <= rjpcsyaw;
  rjpcsyaw <= "";
end psbylsmr;



-- Seed after: 6958070253312702062,2983771601630957889
