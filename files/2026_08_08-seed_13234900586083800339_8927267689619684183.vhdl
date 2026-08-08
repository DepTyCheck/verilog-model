-- Seed: 13234900586083800339,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity sbpmhak is
  port (jlf : out std_logic_vector(3 to 1); mzzqrico : buffer time);
end sbpmhak;

architecture xsjjsnuw of sbpmhak is
  
begin
  -- Single-driven assignments
  mzzqrico <= 8#2_3# ns;
  
  -- Multi-driven assignments
  jlf <= jlf;
end xsjjsnuw;

entity ydeubjbksi is
  port (nfzsv : inout integer; rpqsxsr : buffer integer);
end ydeubjbksi;

architecture mrn of ydeubjbksi is
  
begin
  -- Single-driven assignments
  rpqsxsr <= rpqsxsr;
  nfzsv <= 1;
end mrn;

library ieee;
use ieee.std_logic_1164.all;

entity ba is
  port (rfm : inout boolean_vector(2 to 0); me : out real; th : in std_logic_vector(3 downto 4));
end ba;

library ieee;
use ieee.std_logic_1164.all;

architecture lpatmdryc of ba is
  signal nhmyoawog : integer;
  signal ehyrqmzf : integer;
  signal hffpoya : time;
  signal uuean : std_logic_vector(3 to 1);
begin
  tvxtvc : entity work.sbpmhak
    port map (jlf => uuean, mzzqrico => hffpoya);
  blmrn : entity work.ydeubjbksi
    port map (nfzsv => ehyrqmzf, rpqsxsr => nhmyoawog);
  
  -- Single-driven assignments
  rfm <= rfm;
  me <= me;
end lpatmdryc;

entity dgccapch is
  port (cdoxmiuotd : buffer boolean_vector(0 downto 1));
end dgccapch;

library ieee;
use ieee.std_logic_1164.all;

architecture yvjjz of dgccapch is
  signal ffcqf : time;
  signal ajpdouoodx : std_logic_vector(3 to 1);
  signal nxawvyufv : integer;
  signal ifps : integer;
begin
  doohipamhr : entity work.ydeubjbksi
    port map (nfzsv => ifps, rpqsxsr => nxawvyufv);
  mb : entity work.sbpmhak
    port map (jlf => ajpdouoodx, mzzqrico => ffcqf);
  
  -- Single-driven assignments
  cdoxmiuotd <= (others => TRUE);
  
  -- Multi-driven assignments
  ajpdouoodx <= (others => '0');
  ajpdouoodx <= ajpdouoodx;
  ajpdouoodx <= ajpdouoodx;
end yvjjz;



-- Seed after: 6716355286179948063,8927267689619684183
