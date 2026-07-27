-- Seed: 6929021520181989652,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity vntkcgee is
  port (ywjrt : buffer real; usfpazqgc : linkage std_logic_vector(2 to 2); ryhxtx : inout time_vector(1 downto 1); zv : in time);
end vntkcgee;

architecture isilnv of vntkcgee is
  
begin
  -- Single-driven assignments
  ywjrt <= 8#7_2.272#;
end isilnv;

entity ygs is
  port (yx : inout integer);
end ygs;

library ieee;
use ieee.std_logic_1164.all;

architecture q of ygs is
  signal utjxe : time_vector(1 downto 1);
  signal ccrgbrc : std_logic_vector(2 to 2);
  signal auierizeut : real;
  signal okuwjpss : time;
  signal xsmax : time_vector(1 downto 1);
  signal l : std_logic_vector(2 to 2);
  signal bsclcwue : real;
begin
  iczjme : entity work.vntkcgee
    port map (ywjrt => bsclcwue, usfpazqgc => l, ryhxtx => xsmax, zv => okuwjpss);
  jozc : entity work.vntkcgee
    port map (ywjrt => auierizeut, usfpazqgc => ccrgbrc, ryhxtx => utjxe, zv => okuwjpss);
  
  -- Multi-driven assignments
  l <= l;
end q;

library ieee;
use ieee.std_logic_1164.all;

entity tkfvfofqbi is
  port (banqpf : out time; onvb : in std_logic; gds : buffer boolean; zcilmlipk : in std_logic);
end tkfvfofqbi;

library ieee;
use ieee.std_logic_1164.all;

architecture smis of tkfvfofqbi is
  signal dazjejo : time_vector(1 downto 1);
  signal q : real;
  signal m : integer;
  signal btnrpgybhh : time_vector(1 downto 1);
  signal flyxq : real;
  signal ieqb : time_vector(1 downto 1);
  signal p : std_logic_vector(2 to 2);
  signal vbxnhgxbns : real;
begin
  cbq : entity work.vntkcgee
    port map (ywjrt => vbxnhgxbns, usfpazqgc => p, ryhxtx => ieqb, zv => banqpf);
  kcwokbn : entity work.vntkcgee
    port map (ywjrt => flyxq, usfpazqgc => p, ryhxtx => btnrpgybhh, zv => banqpf);
  vmoxb : entity work.ygs
    port map (yx => m);
  zykvlvo : entity work.vntkcgee
    port map (ywjrt => q, usfpazqgc => p, ryhxtx => dazjejo, zv => banqpf);
  
  -- Single-driven assignments
  gds <= gds;
  banqpf <= banqpf;
  
  -- Multi-driven assignments
  p <= p;
  p <= p;
end smis;



-- Seed after: 2620811639533079675,662889661651915549
