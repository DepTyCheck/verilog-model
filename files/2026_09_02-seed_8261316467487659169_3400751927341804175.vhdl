-- Seed: 8261316467487659169,3400751927341804175

entity vk is
  port (jvkn : buffer time_vector(4 downto 4));
end vk;

architecture qmqyhqwd of vk is
  
begin
  -- Single-driven assignments
  jvkn <= jvkn;
end qmqyhqwd;

entity tl is
  port (degxgwe : in integer);
end tl;

architecture umvdhusykl of tl is
  signal txdgqnlqcu : time_vector(4 downto 4);
  signal rvnoxyad : time_vector(4 downto 4);
begin
  gzkxk : entity work.vk
    port map (jvkn => rvnoxyad);
  obizht : entity work.vk
    port map (jvkn => txdgqnlqcu);
end umvdhusykl;

entity vzd is
  port (syaomhbng : buffer severity_level; ehdhlzh : out time);
end vzd;

architecture sidblr of vzd is
  signal qrxrckq : time_vector(4 downto 4);
  signal ue : integer;
  signal ptpakyu : integer;
begin
  eiltqih : entity work.tl
    port map (degxgwe => ptpakyu);
  dvsbniwnb : entity work.tl
    port map (degxgwe => ue);
  kmhotsvip : entity work.vk
    port map (jvkn => qrxrckq);
  
  -- Single-driven assignments
  syaomhbng <= syaomhbng;
end sidblr;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (pvyf : inout std_logic_vector(3 to 1); rr : out integer);
end d;

architecture xjldwjsg of d is
  signal pfrwred : time_vector(4 downto 4);
begin
  wfxuoya : entity work.vk
    port map (jvkn => pfrwred);
  
  -- Multi-driven assignments
  pvyf <= pvyf;
  pvyf <= pvyf;
  pvyf <= pvyf;
  pvyf <= "";
end xjldwjsg;



-- Seed after: 13279688015181359350,3400751927341804175
