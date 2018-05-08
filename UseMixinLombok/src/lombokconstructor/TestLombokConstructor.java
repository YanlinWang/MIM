package lombokconstructor;

import lombok.*;

@RequiredArgsConstructor(staticName = "of")
@AllArgsConstructor(access = AccessLevel.PROTECTED)
public class TestLombokConstructor<T> {
    private int x,y;
    @NonNull private T description;
    
    @NoArgsConstructor
    public static class NoArgsExample {
        @NonNull private String field;
    }
}
