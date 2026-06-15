-- Seed: 7570228274575087568,15300320181035395489

entity luqjfbsn is
  port (xgwxil : linkage integer; iflggq : linkage real; ssw : out real; seueqfm : out severity_level);
end luqjfbsn;

architecture fbizstpq of luqjfbsn is
  
begin
  
end fbizstpq;

library ieee;
use ieee.std_logic_1164.all;

entity up is
  port (crybgpwu : out std_logic; iw : linkage std_logic; tawllo : in time);
end up;

architecture loh of up is
  signal jtmztgmd : severity_level;
  signal hiomzalx : real;
  signal htbu : real;
  signal utm : integer;
begin
  spkezndjq : entity work.luqjfbsn
    port map (xgwxil => utm, iflggq => htbu, ssw => hiomzalx, seueqfm => jtmztgmd);
end loh;

library ieee;
use ieee.std_logic_1164.all;

entity sjpezbu is
  port (auedhnm : in std_logic_vector(4 downto 1));
end sjpezbu;

library ieee;
use ieee.std_logic_1164.all;

architecture lhnupxfo of sjpezbu is
  signal b : time;
  signal uzplameid : std_logic;
  signal jnocy : std_logic;
  signal mpaxzdc : severity_level;
  signal bkc : real;
  signal wuhj : real;
  signal ohhrgwtox : integer;
  signal desjr : severity_level;
  signal rczoknx : real;
  signal sgcuycuiwu : real;
  signal qzyqmwhw : integer;
begin
  oqzdxbhxg : entity work.luqjfbsn
    port map (xgwxil => qzyqmwhw, iflggq => sgcuycuiwu, ssw => rczoknx, seueqfm => desjr);
  vytxych : entity work.luqjfbsn
    port map (xgwxil => ohhrgwtox, iflggq => wuhj, ssw => bkc, seueqfm => mpaxzdc);
  hm : entity work.up
    port map (crybgpwu => jnocy, iw => uzplameid, tawllo => b);
  
  -- Single-driven assignments
  b <= 16#9# ps;
  
  -- Multi-driven assignments
  jnocy <= 'L';
  jnocy <= 'X';
  jnocy <= 'L';
  uzplameid <= 'H';
end lhnupxfo;

entity ajhyc is
  port (dnkjrngiz : inout severity_level);
end ajhyc;

library ieee;
use ieee.std_logic_1164.all;

architecture kadbjhqod of ajhyc is
  signal qtruayrxwo : std_logic_vector(4 downto 1);
begin
  uvwohh : entity work.sjpezbu
    port map (auedhnm => qtruayrxwo);
  
  -- Single-driven assignments
  dnkjrngiz <= WARNING;
  
  -- Multi-driven assignments
  qtruayrxwo <= "1U1W";
  qtruayrxwo <= ('X', 'H', 'L', 'Z');
end kadbjhqod;



-- Seed after: 10248088517496113991,15300320181035395489
