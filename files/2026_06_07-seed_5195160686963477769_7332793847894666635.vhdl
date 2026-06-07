-- Seed: 5195160686963477769,7332793847894666635



entity cspscjoln is
  port (hbwtzs : out boolean_vector(3 to 2); qdzvi : inout boolean_vector(0 downto 3); znyrc : in time);
end cspscjoln;



architecture mcnmvnuyw of cspscjoln is
  
begin
  
end mcnmvnuyw;



entity apdokvhf is
  port (wsmcy : inout integer; cptwenwktl : inout bit_vector(1 downto 4));
end apdokvhf;



architecture ncylv of apdokvhf is
  signal kwlylmnav : time;
  signal x : boolean_vector(0 downto 3);
  signal txkzcdpjy : boolean_vector(3 to 2);
begin
  lbtp : entity work.cspscjoln
    port map (hbwtzs => txkzcdpjy, qdzvi => x, znyrc => kwlylmnav);
end ncylv;

library ieee;
use ieee.std_logic_1164.all;

entity xq is
  port (vx : buffer std_logic; k : linkage integer; t : inout std_logic; zpnx : inout real);
end xq;



architecture oxg of xq is
  signal hdzhkss : bit_vector(1 downto 4);
  signal zycolmor : integer;
  signal ymr : bit_vector(1 downto 4);
  signal ifoghcplbq : integer;
  signal adiejdo : bit_vector(1 downto 4);
  signal jyxkwsgyll : integer;
  signal dpmmhzk : time;
  signal aeumluogi : boolean_vector(0 downto 3);
  signal n : boolean_vector(3 to 2);
begin
  clmslm : entity work.cspscjoln
    port map (hbwtzs => n, qdzvi => aeumluogi, znyrc => dpmmhzk);
  liumr : entity work.apdokvhf
    port map (wsmcy => jyxkwsgyll, cptwenwktl => adiejdo);
  os : entity work.apdokvhf
    port map (wsmcy => ifoghcplbq, cptwenwktl => ymr);
  gmnnorwzq : entity work.apdokvhf
    port map (wsmcy => zycolmor, cptwenwktl => hdzhkss);
end oxg;



-- Seed after: 8114098999875507318,7332793847894666635
