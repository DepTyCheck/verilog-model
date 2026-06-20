-- Seed: 17058445366393603010,3924983747739634027

entity hgjw is
  port (gfbw : buffer boolean_vector(4 downto 1); rterbwyki : inout string(4 to 1));
end hgjw;

architecture omkgx of hgjw is
  
begin
  
end omkgx;

library ieee;
use ieee.std_logic_1164.all;

entity parnxkwnb is
  port (qf : inout std_logic_vector(3 to 3));
end parnxkwnb;

architecture ecfeiapgl of parnxkwnb is
  signal sdckubr : string(4 to 1);
  signal tpjrkdmp : boolean_vector(4 downto 1);
  signal ggmtxpyxwp : string(4 to 1);
  signal hqtzzda : boolean_vector(4 downto 1);
begin
  bygfilyeh : entity work.hgjw
    port map (gfbw => hqtzzda, rterbwyki => ggmtxpyxwp);
  coiulz : entity work.hgjw
    port map (gfbw => tpjrkdmp, rterbwyki => sdckubr);
  
  -- Multi-driven assignments
  qf <= "0";
  qf <= "W";
end ecfeiapgl;

entity lv is
  port (vbedhpmxk : out integer; zazbxbzm : in real);
end lv;

library ieee;
use ieee.std_logic_1164.all;

architecture kiuc of lv is
  signal dposyq : std_logic_vector(3 to 3);
  signal ubck : string(4 to 1);
  signal yowktstijq : boolean_vector(4 downto 1);
  signal ratxmaa : std_logic_vector(3 to 3);
begin
  utpj : entity work.parnxkwnb
    port map (qf => ratxmaa);
  qf : entity work.hgjw
    port map (gfbw => yowktstijq, rterbwyki => ubck);
  yvanoud : entity work.parnxkwnb
    port map (qf => dposyq);
  
  -- Single-driven assignments
  vbedhpmxk <= 2#11#;
  
  -- Multi-driven assignments
  ratxmaa <= "X";
end kiuc;



-- Seed after: 12675600060397360018,3924983747739634027
