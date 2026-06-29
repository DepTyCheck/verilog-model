-- Seed: 8683277673927196078,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity bgedynie is
  port ( zxeujkg : linkage real
  ; hubcncgo : buffer std_logic_vector(1 to 4)
  ; cgfeuzk : linkage std_logic_vector(1 to 3)
  ; mlv : buffer bit_vector(3 downto 3)
  );
end bgedynie;

architecture ggpw of bgedynie is
  
begin
  -- Single-driven assignments
  mlv <= (others => '0');
  
  -- Multi-driven assignments
  hubcncgo <= "11ZH";
  hubcncgo <= ('W', 'Z', '1', 'Z');
end ggpw;

library ieee;
use ieee.std_logic_1164.all;

entity jxwwk is
  port (cbwp : inout integer_vector(2 to 4); myzxnmy : in integer; zfqgiogizl : linkage std_logic_vector(4 downto 4); serk : buffer std_logic);
end jxwwk;

architecture iasorc of jxwwk is
  
begin
  -- Single-driven assignments
  cbwp <= (142, 8#1_2_2#, 3_1);
end iasorc;

entity y is
  port (qnvufzzy : buffer real; hfpxg : inout time);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture o of y is
  signal usdp : bit_vector(3 downto 3);
  signal khkd : real;
  signal ckqkecptsf : std_logic;
  signal wz : std_logic_vector(4 downto 4);
  signal mgytho : integer;
  signal jyicp : integer_vector(2 to 4);
  signal ulad : bit_vector(3 downto 3);
  signal hx : std_logic_vector(1 to 3);
  signal mcyas : std_logic_vector(1 to 4);
begin
  vu : entity work.bgedynie
    port map (zxeujkg => qnvufzzy, hubcncgo => mcyas, cgfeuzk => hx, mlv => ulad);
  ceeevfrc : entity work.jxwwk
    port map (cbwp => jyicp, myzxnmy => mgytho, zfqgiogizl => wz, serk => ckqkecptsf);
  li : entity work.bgedynie
    port map (zxeujkg => khkd, hubcncgo => mcyas, cgfeuzk => hx, mlv => usdp);
  
  -- Single-driven assignments
  hfpxg <= 232 ps;
  
  -- Multi-driven assignments
  wz <= (others => 'Z');
  wz <= (others => 'H');
  wz <= "-";
end o;



-- Seed after: 14180432950067944205,17047277710231705797
