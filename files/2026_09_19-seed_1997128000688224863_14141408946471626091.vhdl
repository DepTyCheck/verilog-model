-- Seed: 1997128000688224863,14141408946471626091

entity hstft is
  port (nrzijl : out real; wxatpqb : buffer integer; vsfggkxndc : out real);
end hstft;

architecture bgusn of hstft is
  
begin
  -- Single-driven assignments
  nrzijl <= vsfggkxndc;
end bgusn;

library ieee;
use ieee.std_logic_1164.all;

entity zqhoc is
  port (kdvwjd : in integer_vector(4 downto 1); grhuub : buffer integer; okxbajd : inout std_logic_vector(2 to 3); qm : out integer);
end zqhoc;

architecture niuhyf of zqhoc is
  signal hvcrws : real;
  signal dkp : integer;
  signal lbe : real;
  signal lyv : real;
  signal vjt : real;
begin
  sct : entity work.hstft
    port map (nrzijl => vjt, wxatpqb => qm, vsfggkxndc => lyv);
  rgqxdv : entity work.hstft
    port map (nrzijl => lbe, wxatpqb => dkp, vsfggkxndc => hvcrws);
  
  -- Single-driven assignments
  grhuub <= qm;
end niuhyf;



-- Seed after: 14516526731483399674,14141408946471626091
