-- Seed: 16412403664336574593,10940991575366938685

entity xmui is
  port (tdod : buffer time; f : linkage time; tdrbz : inout time);
end xmui;

architecture uzqtzuopi of xmui is
  
begin
  -- Single-driven assignments
  tdrbz <= tdrbz;
  tdod <= tdrbz;
end uzqtzuopi;

library ieee;
use ieee.std_logic_1164.all;

entity fhslc is
  port (leejyurvkq : in std_logic_vector(3 to 4); qdohbrdrl : buffer integer; cr : inout std_logic_vector(3 downto 0));
end fhslc;

architecture orc of fhslc is
  signal xguxhb : time;
  signal pbxrchmi : time;
  signal iirgiqyht : time;
  signal ckaobcukk : time;
  signal zw : time;
  signal zaifa : time;
  signal thmzv : time;
  signal wiafboqaj : time;
  signal yoqqlilfqe : time;
begin
  jgapjaffv : entity work.xmui
    port map (tdod => yoqqlilfqe, f => wiafboqaj, tdrbz => thmzv);
  shpodlnd : entity work.xmui
    port map (tdod => zaifa, f => zw, tdrbz => ckaobcukk);
  hqvngq : entity work.xmui
    port map (tdod => iirgiqyht, f => pbxrchmi, tdrbz => xguxhb);
  
  -- Single-driven assignments
  qdohbrdrl <= qdohbrdrl;
  
  -- Multi-driven assignments
  cr <= ('W', '1', 'X', '1');
  cr <= ('1', 'X', '-', 'Z');
  cr <= cr;
end orc;



-- Seed after: 18444171206833948607,10940991575366938685
