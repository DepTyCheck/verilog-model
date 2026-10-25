-- Seed: 258055732240610327,13788294582928374589

entity u is
  port (itcrmu : out boolean_vector(1 downto 4); osi : inout bit_vector(3 to 0));
end u;

architecture o of u is
  
begin
  -- Single-driven assignments
  itcrmu <= (others => TRUE);
  osi <= (others => '0');
end o;

entity eneldihhy is
  port (kfylhch : linkage integer);
end eneldihhy;

architecture fshkk of eneldihhy is
  signal bjywgi : bit_vector(3 to 0);
  signal cj : boolean_vector(1 downto 4);
begin
  snlvrplc : entity work.u
    port map (itcrmu => cj, osi => bjywgi);
end fshkk;



-- Seed after: 16289252130428735071,13788294582928374589
