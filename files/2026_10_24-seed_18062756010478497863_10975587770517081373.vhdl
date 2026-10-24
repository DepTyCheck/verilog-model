-- Seed: 18062756010478497863,10975587770517081373

entity sqnq is
  port (o : buffer time; kly : in integer_vector(0 to 2));
end sqnq;

architecture e of sqnq is
  
begin
  -- Single-driven assignments
  o <= o;
end e;



-- Seed after: 17665528324833766545,10975587770517081373
