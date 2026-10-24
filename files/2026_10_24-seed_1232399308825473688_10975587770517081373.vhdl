-- Seed: 1232399308825473688,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity sya is
  port (hsqgpxqxzg : in time; pj : in bit; kh : in std_logic; hpnstcwwq : out std_logic);
end sya;

architecture zx of sya is
  
begin
  -- Multi-driven assignments
  hpnstcwwq <= hpnstcwwq;
  hpnstcwwq <= kh;
  hpnstcwwq <= 'H';
end zx;

library ieee;
use ieee.std_logic_1164.all;

entity uxylb is
  port (oc : buffer real; zqdpjx : buffer time; hpsg : buffer std_logic; pr : in real);
end uxylb;

library ieee;
use ieee.std_logic_1164.all;

architecture jfawhhv of uxylb is
  signal rpzrpye : time;
  signal wuqo : std_logic;
  signal tuaaisvo : bit;
  signal dy : time;
  signal wimwjwk : time;
  signal xiy : std_logic;
  signal c : std_logic;
  signal yimqczxezt : bit;
begin
  fua : entity work.sya
    port map (hsqgpxqxzg => zqdpjx, pj => yimqczxezt, kh => c, hpnstcwwq => xiy);
  wunfuwjz : entity work.sya
    port map (hsqgpxqxzg => wimwjwk, pj => yimqczxezt, kh => hpsg, hpnstcwwq => hpsg);
  lfvebgibb : entity work.sya
    port map (hsqgpxqxzg => dy, pj => tuaaisvo, kh => wuqo, hpnstcwwq => hpsg);
  zsthwly : entity work.sya
    port map (hsqgpxqxzg => rpzrpye, pj => yimqczxezt, kh => wuqo, hpnstcwwq => hpsg);
  
  -- Single-driven assignments
  zqdpjx <= 16#A_D_B_D# ns;
  dy <= wimwjwk;
  oc <= pr;
  
  -- Multi-driven assignments
  hpsg <= '1';
  hpsg <= '-';
  hpsg <= hpsg;
end jfawhhv;



-- Seed after: 748322528340668912,10975587770517081373
