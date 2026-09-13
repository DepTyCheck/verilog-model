-- Seed: 674821022650242834,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity ghvftspxcl is
  port (xakg : linkage std_logic; b : out time; eqjhnmmx : in bit; enkguchor : linkage std_logic_vector(3 downto 0));
end ghvftspxcl;

architecture vizzw of ghvftspxcl is
  
begin
  -- Single-driven assignments
  b <= b;
end vizzw;

library ieee;
use ieee.std_logic_1164.all;

entity ffitgecca is
  port (aap : out std_logic_vector(2 to 1); btqsdtm : in std_logic_vector(2 downto 2));
end ffitgecca;

library ieee;
use ieee.std_logic_1164.all;

architecture zzbxuvijxj of ffitgecca is
  signal oaroacocev : std_logic_vector(3 downto 0);
  signal myimdd : bit;
  signal jjtpk : time;
  signal fueehgycue : std_logic_vector(3 downto 0);
  signal ytoato : bit;
  signal ymuskxirqk : time;
  signal gryo : std_logic;
begin
  nrvwaqwzl : entity work.ghvftspxcl
    port map (xakg => gryo, b => ymuskxirqk, eqjhnmmx => ytoato, enkguchor => fueehgycue);
  gjolbvvmhl : entity work.ghvftspxcl
    port map (xakg => gryo, b => jjtpk, eqjhnmmx => myimdd, enkguchor => oaroacocev);
  
  -- Single-driven assignments
  ytoato <= ytoato;
  myimdd <= myimdd;
  
  -- Multi-driven assignments
  oaroacocev <= fueehgycue;
  aap <= aap;
  aap <= aap;
end zzbxuvijxj;

entity megbzyenar is
  port (impxub : buffer time; y : out bit);
end megbzyenar;

library ieee;
use ieee.std_logic_1164.all;

architecture xeb of megbzyenar is
  signal qjbivtqtv : std_logic_vector(3 downto 0);
  signal mffmtg : std_logic;
begin
  fvuyb : entity work.ghvftspxcl
    port map (xakg => mffmtg, b => impxub, eqjhnmmx => y, enkguchor => qjbivtqtv);
  
  -- Single-driven assignments
  y <= '0';
end xeb;

entity awblnymrx is
  port (dess : inout integer; pcex : buffer bit);
end awblnymrx;

library ieee;
use ieee.std_logic_1164.all;

architecture ocrku of awblnymrx is
  signal dt : std_logic_vector(3 downto 0);
  signal spuobkuxc : time;
  signal scbb : bit;
  signal zz : time;
  signal q : bit;
  signal rtccxfq : time;
  signal uevzvkoh : std_logic_vector(3 downto 0);
  signal cr : time;
  signal klcxqsr : std_logic;
begin
  vrkkw : entity work.ghvftspxcl
    port map (xakg => klcxqsr, b => cr, eqjhnmmx => pcex, enkguchor => uevzvkoh);
  m : entity work.ghvftspxcl
    port map (xakg => klcxqsr, b => rtccxfq, eqjhnmmx => q, enkguchor => uevzvkoh);
  enpyyznp : entity work.megbzyenar
    port map (impxub => zz, y => scbb);
  ztwfc : entity work.ghvftspxcl
    port map (xakg => klcxqsr, b => spuobkuxc, eqjhnmmx => scbb, enkguchor => dt);
  
  -- Single-driven assignments
  dess <= dess;
  q <= pcex;
  pcex <= '1';
  
  -- Multi-driven assignments
  uevzvkoh <= dt;
end ocrku;



-- Seed after: 2162161727277402034,10754487200446211253
