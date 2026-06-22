-- Seed: 1015402814696799111,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity pnkigliz is
  port (vjl : buffer boolean; okaj : inout integer; vnd : in std_logic_vector(2 to 0); zjgpntrreq : inout real);
end pnkigliz;

architecture ceem of pnkigliz is
  
begin
  -- Single-driven assignments
  vjl <= TRUE;
  okaj <= 0;
end ceem;

entity ybw is
  port (zbe : buffer time);
end ybw;

library ieee;
use ieee.std_logic_1164.all;

architecture lik of ybw is
  signal kprsvospaf : real;
  signal eezughrpf : std_logic_vector(2 to 0);
  signal lrgjxkv : integer;
  signal cn : boolean;
  signal aewgbsgcv : real;
  signal xbhgezlmld : std_logic_vector(2 to 0);
  signal pmzy : integer;
  signal lnuihn : boolean;
begin
  dtonnhi : entity work.pnkigliz
    port map (vjl => lnuihn, okaj => pmzy, vnd => xbhgezlmld, zjgpntrreq => aewgbsgcv);
  oqufxkmtp : entity work.pnkigliz
    port map (vjl => cn, okaj => lrgjxkv, vnd => eezughrpf, zjgpntrreq => kprsvospaf);
  
  -- Single-driven assignments
  zbe <= 16#03E.4_2# us;
end lik;



-- Seed after: 1156733940366012687,13479070923501788437
