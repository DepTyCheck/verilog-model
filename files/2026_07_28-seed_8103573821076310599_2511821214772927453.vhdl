-- Seed: 8103573821076310599,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity qqkcosvv is
  port ( wet : buffer integer_vector(0 to 2)
  ; q : linkage std_logic_vector(4 downto 1)
  ; ubwnm : buffer time_vector(4 downto 4)
  ; sechfz : out real_vector(0 to 1)
  );
end qqkcosvv;

architecture pa of qqkcosvv is
  
begin
  -- Single-driven assignments
  wet <= wet;
  ubwnm <= ubwnm;
  sechfz <= sechfz;
end pa;

entity fispmtmv is
  port (ipseszeq : in real; jhuwntcrac : in time; ulr : buffer boolean);
end fispmtmv;

library ieee;
use ieee.std_logic_1164.all;

architecture xdcihhvlkz of fispmtmv is
  signal pmjvgcibi : real_vector(0 to 1);
  signal sbgypnaf : time_vector(4 downto 4);
  signal hoaxxu : integer_vector(0 to 2);
  signal z : real_vector(0 to 1);
  signal asiqacsvzw : time_vector(4 downto 4);
  signal zquyo : integer_vector(0 to 2);
  signal gpoazl : real_vector(0 to 1);
  signal rypdy : time_vector(4 downto 4);
  signal iksubt : std_logic_vector(4 downto 1);
  signal hek : integer_vector(0 to 2);
begin
  ochthhajeu : entity work.qqkcosvv
    port map (wet => hek, q => iksubt, ubwnm => rypdy, sechfz => gpoazl);
  bpvo : entity work.qqkcosvv
    port map (wet => zquyo, q => iksubt, ubwnm => asiqacsvzw, sechfz => z);
  zdivfe : entity work.qqkcosvv
    port map (wet => hoaxxu, q => iksubt, ubwnm => sbgypnaf, sechfz => pmjvgcibi);
  
  -- Single-driven assignments
  ulr <= FALSE;
end xdcihhvlkz;



-- Seed after: 15736562891783041352,2511821214772927453
