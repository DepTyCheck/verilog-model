-- Seed: 17335720714580600403,13857275728440271305

entity aap is
  port (wdx : out character; xohlzms : linkage time; homuwgkbs : in time; dnmurir : inout bit_vector(0 to 3));
end aap;

architecture qoemzu of aap is
  
begin
  
end qoemzu;

entity firwcjh is
  port (ttv : in integer; kbqaugpraj : inout bit_vector(4 downto 0); kf : out real);
end firwcjh;

architecture hry of firwcjh is
  signal tptojx : bit_vector(0 to 3);
  signal ldxdu : time;
  signal trlpqtwwii : time;
  signal euniyrjb : character;
begin
  huhzvgwuf : entity work.aap
    port map (wdx => euniyrjb, xohlzms => trlpqtwwii, homuwgkbs => ldxdu, dnmurir => tptojx);
end hry;

library ieee;
use ieee.std_logic_1164.all;

entity vldyskqgd is
  port (luwwg : out boolean; heutlwa : out time; mqezzwkwyp : linkage std_logic);
end vldyskqgd;

architecture itb of vldyskqgd is
  signal xv : bit_vector(0 to 3);
  signal udnvdr : character;
  signal gyx : bit_vector(0 to 3);
  signal mxaxxyn : time;
  signal xluf : character;
  signal x : real;
  signal fzithwrklr : bit_vector(4 downto 0);
  signal ywjmyrr : integer;
begin
  yhlyeo : entity work.firwcjh
    port map (ttv => ywjmyrr, kbqaugpraj => fzithwrklr, kf => x);
  aend : entity work.aap
    port map (wdx => xluf, xohlzms => mxaxxyn, homuwgkbs => mxaxxyn, dnmurir => gyx);
  rzwxkrbo : entity work.aap
    port map (wdx => udnvdr, xohlzms => heutlwa, homuwgkbs => mxaxxyn, dnmurir => xv);
  
  -- Single-driven assignments
  luwwg <= TRUE;
  ywjmyrr <= 2233;
end itb;

entity wzkz is
  port (lemfwom : out integer);
end wzkz;

architecture ibazqb of wzkz is
  
begin
  
end ibazqb;



-- Seed after: 2186606408921543084,13857275728440271305
