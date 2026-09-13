-- Seed: 1187303352030388973,10754487200446211253

entity cnjgexxnh is
  port (mtsm : inout time; top : in time_vector(4 to 4); ja : linkage real_vector(4 downto 2));
end cnjgexxnh;

architecture uvcrnldtm of cnjgexxnh is
  
begin
  -- Single-driven assignments
  mtsm <= mtsm;
end uvcrnldtm;

library ieee;
use ieee.std_logic_1164.all;

entity jrp is
  port (gx : out std_logic; tjxxteg : buffer bit; lyedkxhtyk : buffer bit_vector(2 downto 3));
end jrp;

architecture tejiprrnhr of jrp is
  signal zqeb : real_vector(4 downto 2);
  signal abyuucny : time_vector(4 to 4);
  signal htlw : time;
  signal hjseglyqi : real_vector(4 downto 2);
  signal znnawqiez : time_vector(4 to 4);
  signal rnastg : time;
  signal zeckooz : real_vector(4 downto 2);
  signal cmqdoq : time_vector(4 to 4);
  signal yw : time;
  signal xiemb : real_vector(4 downto 2);
  signal fx : time_vector(4 to 4);
  signal npr : time;
begin
  gmetgw : entity work.cnjgexxnh
    port map (mtsm => npr, top => fx, ja => xiemb);
  vsvmzsf : entity work.cnjgexxnh
    port map (mtsm => yw, top => cmqdoq, ja => zeckooz);
  gdfbbytkpr : entity work.cnjgexxnh
    port map (mtsm => rnastg, top => znnawqiez, ja => hjseglyqi);
  qsfysbvxc : entity work.cnjgexxnh
    port map (mtsm => htlw, top => abyuucny, ja => zqeb);
  
  -- Single-driven assignments
  znnawqiez <= fx;
  
  -- Multi-driven assignments
  gx <= 'L';
  gx <= gx;
  gx <= '-';
end tejiprrnhr;



-- Seed after: 17479914408866632287,10754487200446211253
