-- Seed: 7793329056521273811,3924983747739634027

entity lscafvyvqw is
  port (ut : buffer time_vector(1 downto 4));
end lscafvyvqw;

architecture hltaerduht of lscafvyvqw is
  
begin
  -- Single-driven assignments
  ut <= (others => 0 ns);
end hltaerduht;

entity oxtypur is
  port (oxxtvcyo : buffer integer; mysidi : inout time);
end oxtypur;

architecture izfmjcox of oxtypur is
  
begin
  -- Single-driven assignments
  mysidi <= 16#4_3# fs;
  oxxtvcyo <= 420;
end izfmjcox;

library ieee;
use ieee.std_logic_1164.all;

entity iek is
  port (elxsax : buffer std_logic);
end iek;

architecture o of iek is
  signal iurezyqc : time_vector(1 downto 4);
  signal wzqqk : time_vector(1 downto 4);
  signal zrvyfk : time_vector(1 downto 4);
  signal uxe : time_vector(1 downto 4);
begin
  szqsidc : entity work.lscafvyvqw
    port map (ut => uxe);
  fngo : entity work.lscafvyvqw
    port map (ut => zrvyfk);
  lqu : entity work.lscafvyvqw
    port map (ut => wzqqk);
  goywejrt : entity work.lscafvyvqw
    port map (ut => iurezyqc);
  
  -- Multi-driven assignments
  elxsax <= 'Z';
  elxsax <= 'U';
  elxsax <= 'U';
  elxsax <= '-';
end o;

entity omqdiy is
  port (btmjllcpce : out real_vector(1 downto 3); uyyohgou : linkage string(2 downto 2); qtgi : out time);
end omqdiy;

library ieee;
use ieee.std_logic_1164.all;

architecture yxc of omqdiy is
  signal ael : time;
  signal l : integer;
  signal i : std_logic;
  signal agtfhh : time_vector(1 downto 4);
begin
  ohpbtgj : entity work.lscafvyvqw
    port map (ut => agtfhh);
  rqmybmqy : entity work.iek
    port map (elxsax => i);
  twpayq : entity work.oxtypur
    port map (oxxtvcyo => l, mysidi => ael);
  
  -- Single-driven assignments
  qtgi <= 16#A_9_9.5E273# ns;
end yxc;



-- Seed after: 701325535039072708,3924983747739634027
